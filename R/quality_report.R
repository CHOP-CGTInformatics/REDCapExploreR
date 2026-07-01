#' @title Build a REDCap data quality report
#'
#' @description
#' `build_quality_report()` normalizes raw REDCap data or a REDCapTidieR
#' supertibble, applies general data quality heuristics, and returns findings,
#' summaries, and interpreted metadata.
#'
#' @param data A raw REDCap data frame, path to a REDCap CSV export, REDCapTidieR
#'   supertibble, or a list returned by `pull_redcap_project()`.
#' @param metadata A REDCap metadata/data dictionary data frame or CSV path. This
#'   is required for raw data unless `data` is a REDCapTidieR supertibble with
#'   enough embedded metadata.
#' @param events Optional REDCap events data frame or CSV path.
#' @param instruments Optional REDCap instruments data frame or CSV path.
#' @param repeating_instruments Optional REDCap repeating instruments/events data
#'   frame or CSV path.
#' @param redcap_uri REDCap API URI. Required when `source = "api"`.
#' @param token REDCap API token. Required when `source = "api"`.
#' @param source Input source type. Use `"auto"` to infer from supplied
#'   arguments.
#' @param checks Character vector of check groups to run.
#' @param sparse_threshold Missingness threshold used to flag unexpectedly sparse
#'   fields.
#' @param outlier_iqr_multiplier Multiplier used for IQR-based numeric outlier
#'   detection.
#' @param progress Progress display mode. Use `"auto"` to show progress only in
#'   interactive sessions, `"none"` to suppress progress, or `"show"` to force
#'   progress output.
#'
#' @returns A list with `findings`, `summaries`, and `metadata` elements.
#'   `summaries$project$raw_row_count` is only present for raw REDCap inputs and
#'   equals `nrow()` for the raw export. `summaries$project$supertbl_row_count`
#'   is only present for REDCapTidieR supertibble inputs and equals the number of
#'   rows after binding the instrument-level tibbles. `summaries$project$field_count`
#'   is the count of distinct standardized metadata field names.
#'
#' @examples
#' \dontrun{
#' report <- build_quality_report(
#'   data = "redcap_export.csv",
#'   metadata = "redcap_data_dictionary.csv"
#' )
#'
#' supertbl <- REDCapTidieR::read_redcap(
#'   redcap_uri = Sys.getenv("REDCAP_URI"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#'
#' report <- build_quality_report(supertbl)
#' }
#'
#' @export
build_quality_report <- function(data = NULL,
                                 metadata = NULL,
                                 events = NULL,
                                 instruments = NULL,
                                 repeating_instruments = NULL,
                                 redcap_uri = NULL,
                                 token = NULL,
                                 source = c("auto", "supertbl", "raw", "api"),
                                 checks = c(
                                   "missingness",
                                   "metadata",
                                   "outliers",
                                   "operational",
                                   "consistency"
                                 ),
                                 sparse_threshold = 0.95,
                                 outlier_iqr_multiplier = 3,
                                 progress = c("auto", "none", "show")) {
  source <- match.arg(source)
  progress <- match.arg(progress)
  checks <- match.arg(
    checks,
    choices = c("missingness", "metadata", "outliers", "operational", "consistency"),
    several.ok = TRUE
  )

  progress_enabled <- get_progress_enabled(progress)
  progress_force <- progress == "show"
  progress_bar <- get_progress_bar(
    enabled = progress_enabled,
    total_steps = get_report_progress_steps(checks)
  )
  on.exit(
    if (progress_enabled) {
      close_report_progress(progress_bar, progress_enabled)
    },
    add = TRUE
  )

  get_validate_thresholds(sparse_threshold, outlier_iqr_multiplier)

  project <- get_quality_project(
    data = data,
    metadata = metadata,
    events = events,
    instruments = instruments,
    repeating_instruments = repeating_instruments,
    redcap_uri = redcap_uri,
    token = token,
    source = source
  )
  update_report_progress(progress_bar, progress_enabled, progress_force, "Normalized input")

  field_summary <- get_field_summary(project)
  update_report_progress(progress_bar, progress_enabled, progress_force, "Built field summaries")

  check_fns <- list(
    missingness = \(...) get_missingness_findings(project, field_summary, sparse_threshold),
    metadata = \(...) get_metadata_findings(project),
    outliers = \(...) get_outlier_findings(project, outlier_iqr_multiplier),
    operational = \(...) get_operational_findings(project),
    consistency = \(...) get_consistency_findings(project)
  )

  findings <- bind_rows(map(checks, \(check) {
    out <- check_fns[[check]]()
    update_report_progress(
      progress_bar,
      progress_enabled,
      progress_force,
      paste("Ran", check, "checks")
    )
    out
  }))

  if (nrow(findings) == 0) {
    findings <- get_empty_findings()
  } else {
    findings <- findings |>
      mutate(finding_id = row_number()) |>
      relocate("finding_id")
  }
  update_report_progress(progress_bar, progress_enabled, progress_force, "Assembled findings")

  summaries <- list(
    project = get_project_summary(project, findings, field_summary),
    forms = get_form_summary(project, field_summary),
    fields = field_summary,
    records = get_record_summary(project, findings),
    events = get_event_summary(project)
  )
  update_report_progress(progress_bar, progress_enabled, progress_force, "Built summaries")

  report_metadata <- list(
    fields = project$metadata,
    forms = get_form_metadata(project),
    events = project$events,
    instruments = project$instruments,
    repeating_instruments = project$repeating_instruments,
    instrument_structure = project$instrument_structure,
    choices = get_choice_metadata(project),
    validation = get_validation_metadata(project),
    branching = get_branching_metadata(project),
    record_id_field = project$record_id_field,
    source = project$source
  )
  update_report_progress(progress_bar, progress_enabled, progress_force, "Built metadata")

  report <- list(
    findings = findings,
    summaries = summaries,
    metadata = report_metadata
  )

  class(report) <- c("redcap_quality_report", class(report))
  report
}

#' @title Pull REDCap project data for quality reporting
#'
#' @description
#' Retrieves records and project metadata from the REDCap API using REDCapR.
#'
#' @param redcap_uri REDCap API URI.
#' @param token REDCap API token.
#'
#' @returns A list containing raw records, metadata, events, instruments, and
#'   repeating instrument configuration.
#'
#' @export
pull_redcap_project <- function(redcap_uri, token) {
  if (missing(redcap_uri) || missing(token) || redcap_uri == "" || token == "") {
    cli_abort("{.arg redcap_uri} and {.arg token} are required for API pulls.")
  }

  data_result <- redcap_read_oneshot(redcap_uri = redcap_uri, token = token)
  metadata_result <- redcap_metadata_read(redcap_uri = redcap_uri, token = token)

  list(
    data = get_redcapr_data(data_result, "records"),
    metadata = get_redcapr_data(metadata_result, "metadata"),
    events = get_optional_redcapr_data(redcap_event_read, redcap_uri, token),
    instruments = get_optional_redcapr_data(redcap_instruments, redcap_uri, token),
    repeating_instruments = get_optional_redcapr_data(
      redcap_instrument_repeating,
      redcap_uri,
      token
    )
  )
}

#' @export
print.redcap_quality_report <- function(x, ...) {
  project <- x$summaries$project

  cat("<REDCap quality report>\n")
  cat("Records: ", project$record_count, "\n", sep = "")
  cat("Fields: ", project$field_count, "\n", sep = "")
  cat("Findings: ", project$finding_count, "\n", sep = "")
  invisible(x)
}

get_progress_enabled <- function(progress) {
  if (progress == "show") {
    return(TRUE)
  }

  if (progress == "none") {
    return(FALSE)
  }

  interactive()
}

get_report_progress_steps <- function(checks) {
  length(checks) + 5L
}

get_progress_bar <- function(enabled, total_steps) {
  if (!enabled) {
    return(NULL)
  }

  progress_env <- new.env(parent = parent.frame())
  progress_id <- cli_progress_bar(
    name = "Building REDCap quality report",
    total = total_steps,
    format = "{cli::pb_name}{cli::pb_bar} {cli::pb_percent} | {cli::pb_status}",
    format_done = paste0("{cli::pb_name}{cli::pb_percent} | ", symbol$tick, " Complete"),
    clear = FALSE,
    auto_terminate = FALSE,
    .envir = progress_env
  )
  list(id = progress_id, envir = progress_env)
}

update_report_progress <- function(progress_bar, enabled, force, message) {
  if (!enabled) {
    return(invisible(NULL))
  }

  tryCatch(
    cli_progress_update(
      id = progress_bar$id,
      inc = 1,
      status = message,
      force = force,
      .envir = progress_bar$envir
    ),
    error = function(cnd) NULL
  )
  invisible(NULL)
}

close_report_progress <- function(progress_bar, enabled) {
  if (!enabled) {
    return(invisible(NULL))
  }

  tryCatch(
    cli_progress_done(id = progress_bar$id, .envir = progress_bar$envir),
    error = function(cnd) NULL
  )
  invisible(NULL)
}

get_quality_project <- function(data,
                                metadata,
                                events,
                                instruments,
                                repeating_instruments,
                                redcap_uri,
                                token,
                                source) {
  if (source == "api" || (source == "auto" && !is.null(redcap_uri))) {
    pulled <- pull_redcap_project(redcap_uri = redcap_uri, token = token)
    return(get_quality_project(
      data = pulled,
      metadata = NULL,
      events = NULL,
      instruments = NULL,
      repeating_instruments = NULL,
      redcap_uri = NULL,
      token = NULL,
      source = "raw"
    ))
  }

  if (source == "auto") {
    source <- get_inferred_source(data)
  }

  if (source == "supertbl") {
    out <- get_supertbl_project(data, metadata)
  } else {
    out <- get_raw_project(
      data = data,
      metadata = metadata,
      events = events,
      instruments = instruments,
      repeating_instruments = repeating_instruments
    )
  }

  out$record_id_field <- get_record_id_field_report(out$data, out$metadata)
  out
}

get_inferred_source <- function(data) {
  if (is_redcap_project_list(data)) {
    return("raw")
  }

  if (is.data.frame(data) && all(c("redcap_form_name", "redcap_data") %in% names(data))) {
    return("supertbl")
  }

  "raw"
}

get_raw_project <- function(data,
                            metadata,
                            events = NULL,
                            instruments = NULL,
                            repeating_instruments = NULL) {
  if (is_redcap_project_list(data)) {
    metadata <- data$metadata
    events <- data$events
    instruments <- data$instruments
    repeating_instruments <- data$repeating_instruments
    data <- data$data
  }

  if (is.null(data)) {
    cli_abort("{.arg data} is required.")
  }

  if (is.null(metadata)) {
    cli_abort("{.arg metadata} is required for raw REDCap data.")
  }

  out <- list(
    data = as_tibble(get_input_table(data)),
    metadata = get_standard_metadata(metadata),
    events = as_tibble(get_optional_table(events)),
    instruments = as_tibble(get_optional_table(instruments)),
    repeating_instruments = as_tibble(get_optional_table(repeating_instruments)),
    instrument_structure = tibble(),
    source = "raw"
  )

  out <- get_drop_descriptive_fields(out)
  get_validate_project(out)
}

get_drop_descriptive_fields <- function(project) {
  descriptive_fields <- project$metadata |>
    filter(tolower(.data$field_type) %in% c("descriptive", "descriptive_text")) |>
    pull(.data$field_name)

  if (length(descriptive_fields) == 0) {
    return(project)
  }

  project$metadata <- project$metadata |>
    filter(!.data$field_name %in% descriptive_fields)
  project$data <- project$data |>
    select(-any_of(descriptive_fields))

  project
}

get_supertbl_project <- function(supertbl, metadata = NULL) {
  if (!is.data.frame(supertbl) || !"redcap_data" %in% names(supertbl)) {
    cli_abort("{.arg data} must be a REDCapTidieR supertibble.")
  }

  data_tbls <- map2(
    supertbl$redcap_data,
    seq_along(supertbl$redcap_data),
    \(tbl, index) {
      form_name <- supertbl$redcap_form_name[[index]]
      tbl <- as_tibble(tbl)
      if (!"redcap_form_name" %in% names(tbl)) {
        tbl$redcap_form_name <- form_name
      }
      if (!"redcap_form_label" %in% names(tbl) && "redcap_form_label" %in% names(supertbl)) {
        tbl$redcap_form_label <- supertbl$redcap_form_label[[index]]
      }
      tbl
    }
  )

  events <- get_supertbl_nested_table(supertbl, "redcap_events") |>
    get_canonical_event_data()

  data <- bind_rows(data_tbls) |>
    get_canonical_checkbox_data() |>
    get_canonical_event_data(events)

  metadata_tbl <- if (!is.null(metadata)) {
    get_standard_metadata(metadata)
  } else if ("redcap_metadata" %in% names(supertbl)) {
    get_supertbl_metadata(supertbl)
  } else {
    get_inferred_supertbl_metadata(supertbl, data)
  }
  metadata_tbl <- metadata_tbl |>
    get_canonical_checkbox_metadata() |>
    get_deduplicated_metadata()

  out <- list(
    data = data,
    metadata = metadata_tbl,
    events = events,
    instruments = tibble(),
    repeating_instruments = get_supertbl_nested_table(supertbl, "redcap_repeating_instruments"),
    instrument_structure = get_supertbl_instrument_structure(supertbl),
    source = "supertbl"
  )

  get_validate_project(out)
}

get_supertbl_metadata <- function(supertbl) {
  metadata <- bind_rows(map2(
    supertbl$redcap_metadata,
    seq_along(supertbl$redcap_metadata),
    \(tbl, index) {
      tbl <- as_tibble(tbl)
      if (!"form_name" %in% names(tbl)) {
        tbl$form_name <- supertbl$redcap_form_name[[index]]
      } else {
        tbl$form_name[get_is_missing(tbl$form_name)] <- supertbl$redcap_form_name[[index]]
      }
      if (!"form_label" %in% names(tbl) && "redcap_form_label" %in% names(supertbl)) {
        tbl$form_label <- supertbl$redcap_form_label[[index]]
      }
      tbl
    }
  ))

  metadata |>
    get_standard_metadata()
}

get_canonical_checkbox_metadata <- function(metadata) {
  checkbox_rows <- str_detect(metadata$field_name, "___")
  if (!any(checkbox_rows)) {
    return(metadata)
  }

  metadata |>
    mutate(
      .is_checkbox_choice = checkbox_rows,
      .checkbox_parent_field = if_else(
        .data$.is_checkbox_choice,
        sub("___.*$", "", .data$field_name),
        .data$field_name
      )
    ) |>
    group_by(.data$.checkbox_parent_field) |>
    mutate(
      field_name = if_else(.data$.is_checkbox_choice, .data$.checkbox_parent_field, .data$field_name),
      field_type = if_else(.data$.is_checkbox_choice, "checkbox", .data$field_type),
      field_label = if_else(
        .data$.is_checkbox_choice,
        get_checkbox_parent_label(.data$field_label),
        .data$field_label
      )
    ) |>
    ungroup() |>
    select(-".is_checkbox_choice", -".checkbox_parent_field")
}

get_checkbox_parent_label <- function(field_label) {
  labels <- unique(field_label[!get_is_missing(field_label)])
  if (length(labels) == 0) {
    return(NA_character_)
  }

  colon_prefix <- sub("^(.*?:)\\s+.+$", "\\1", labels)
  if (length(unique(colon_prefix)) == 1 && colon_prefix[[1]] != labels[[1]]) {
    return(colon_prefix[[1]])
  }

  labels[[1]]
}

get_deduplicated_metadata <- function(metadata) {
  metadata |>
    distinct(.data$field_name, .keep_all = TRUE)
}

get_supertbl_nested_table <- function(supertbl, column) {
  if (!column %in% names(supertbl)) {
    return(tibble())
  }

  bind_rows(map2(
    supertbl[[column]],
    supertbl$redcap_form_name,
    \(tbl, form_name) {
      tbl <- as_tibble(tbl)
      if (!"redcap_form_name" %in% names(tbl)) {
        tbl$redcap_form_name <- form_name
      }
      tbl
    }
  ))
}

get_canonical_checkbox_data <- function(data) {
  checkbox_columns <- names(data)[str_detect(names(data), "___")]
  if (length(checkbox_columns) == 0) {
    return(data)
  }

  canonical_columns <- unique(sub("(_raw|_label)$", "", checkbox_columns))

  for (column in canonical_columns) {
    raw_column <- paste0(column, "_raw")
    label_column <- paste0(column, "_label")

    data[[column]] <- if (raw_column %in% names(data)) {
      get_checkbox_indicator(data[[raw_column]])
    } else if (column %in% names(data)) {
      get_checkbox_indicator(data[[column]])
    } else if (label_column %in% names(data)) {
      if_else(get_is_missing(data[[label_column]]), 0L, 1L)
    } else {
      data[[column]]
    }
  }

  helper_columns <- names(data)[
    str_detect(names(data), "___") &
      str_detect(names(data), "(_raw|_label)$")
  ]
  data |>
    select(-any_of(helper_columns))
}

get_checkbox_indicator <- function(value) {
  if_else(get_is_checked(value), 1L, 0L)
}

get_canonical_event_data <- function(data, events = NULL) {
  data <- get_fill_event_arm(data, events)

  if (!all(c("redcap_event", "redcap_arm") %in% names(data))) {
    return(data)
  }

  data |>
    mutate(
      redcap_event_name = get_canonical_event_name(
        .data$redcap_event,
        .data$redcap_arm
      )
    )
}

get_fill_event_arm <- function(data, events) {
  if (
    "redcap_arm" %in% names(data) ||
      !"redcap_event" %in% names(data) ||
      is.null(events) ||
      !all(c("redcap_event", "redcap_arm") %in% names(events))
  ) {
    return(data)
  }

  join_fields <- "redcap_event"
  if ("redcap_form_name" %in% names(data) && "redcap_form_name" %in% names(events)) {
    join_fields <- c("redcap_form_name", join_fields)
  }

  event_arms <- events |>
    filter(!get_is_missing(.data$redcap_arm)) |>
    group_by(across(all_of(join_fields))) |>
    filter(n_distinct(.data$redcap_arm) == 1) |>
    summarise(redcap_arm = first(.data$redcap_arm), .groups = "drop")

  if (nrow(event_arms) == 0) {
    return(data)
  }

  data |>
    left_join(event_arms, by = join_fields)
}

get_canonical_event_name <- function(event, arm) {
  event <- as.character(event)
  arm <- as.character(arm)
  arm_number <- sub("^arm_", "", tolower(arm))

  case_when(
    get_is_missing(event) ~ NA_character_,
    str_detect(event, "_arm_[0-9]+$") ~ event,
    get_is_missing(arm_number) ~ event,
    TRUE ~ paste0(event, "_arm_", arm_number)
  )
}

get_supertbl_instrument_structure <- function(supertbl) {
  if (!"structure" %in% names(supertbl)) {
    return(tibble())
  }

  tibble(
    redcap_form_name = supertbl$redcap_form_name,
    structure = as.character(supertbl$structure)
  )
}

get_inferred_supertbl_metadata <- function(supertbl, data) {
  form_labels <- if ("redcap_form_label" %in% names(supertbl)) {
    supertbl$redcap_form_label
  } else {
    supertbl$redcap_form_name
  }

  bind_rows(map2(
    supertbl$redcap_data,
    seq_along(supertbl$redcap_data),
    \(tbl, index) {
      names <- setdiff(names(tbl), c("redcap_form_name", "redcap_form_label"))
      tibble(
        field_name = names,
        form_name = supertbl$redcap_form_name[[index]],
        field_type = NA_character_,
        field_label = names,
        select_choices_or_calculations = NA_character_,
        text_validation_type_or_show_slider_number = NA_character_,
        text_validation_min = NA_character_,
        text_validation_max = NA_character_,
        required_field = FALSE,
        branching_logic = NA_character_,
        identifier = NA_character_,
        form_label = form_labels[[index]]
      )
    }
  )) |>
    distinct(.data$field_name, .data$form_name, .keep_all = TRUE)
}

get_input_table <- function(x) {
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    return(read.csv(x, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (is.data.frame(x)) {
    return(x)
  }

  cli_abort("{.arg data} must be a data frame, CSV path, REDCapTidieR supertibble, or API pull list.")
}

get_optional_table <- function(x) {
  if (is.null(x)) {
    return(tibble())
  }

  get_input_table(x)
}

get_standard_metadata <- function(metadata) {
  metadata <- as_tibble(get_input_table(metadata))
  names(metadata) <- get_clean_names(names(metadata))
  names(metadata) <- get_metadata_names(names(metadata))

  if (!"field_name" %in% names(metadata)) {
    cli_abort("Metadata must include a REDCap field name column.")
  }

  required_columns <- c(
    "field_name",
    "form_name",
    "field_type",
    "field_label",
    "select_choices_or_calculations",
    "text_validation_type_or_show_slider_number",
    "text_validation_min",
    "text_validation_max",
    "required_field",
    "branching_logic",
    "identifier"
  )

  missing_columns <- setdiff(required_columns, names(metadata))
  for (column in missing_columns) {
    metadata[[column]] <- NA_character_
  }

  metadata |>
    mutate(
      field_name = as.character(.data$field_name),
      form_name = if_else(
        get_is_missing(.data$form_name),
        "unknown",
        as.character(.data$form_name)
      ),
      field_type = as.character(.data$field_type),
      field_label = as.character(.data$field_label),
      required_field = tolower(as.character(.data$required_field)) %in%
        c("y", "yes", "1", "true")
    )
}

get_clean_names <- function(names) {
  names <- tolower(names)
  names <- gsub("[^a-z0-9]+", "_", names)
  gsub("^_|_$", "", names)
}

get_metadata_names <- function(names) {
  replacements <- c(
    variable_field_name = "field_name",
    form_name = "form_name",
    field_type = "field_type",
    field_label = "field_label",
    choices_calculations_or_slider_labels = "select_choices_or_calculations",
    choices_calculations_or_slider_labels_ = "select_choices_or_calculations",
    select_choices_or_calculations = "select_choices_or_calculations",
    choices_calculations_or_slider_labels_field_type = "select_choices_or_calculations",
    text_validation_type_or_show_slider_number = "text_validation_type_or_show_slider_number",
    text_validation_min = "text_validation_min",
    text_validation_max = "text_validation_max",
    required_field = "required_field",
    required_field_ = "required_field",
    branching_logic_show_field_only_if = "branching_logic",
    branching_logic = "branching_logic",
    identifier = "identifier",
    identifier_ = "identifier"
  )

  if_else(names %in% names(replacements), replacements[names], names)
}

get_validate_project <- function(project) {
  represented_fields <- project$metadata$field_name[
    map_lgl(project$metadata$field_name, \(field) {
      field %in% names(project$data) ||
        any(startsWith(names(project$data), paste0(field, "___")))
    })
  ]
  missing_fields <- setdiff(project$metadata$field_name, represented_fields)
  if (length(missing_fields) > 0) {
    cli_warn(c(
      "Some metadata fields are not present in the data.",
      "x" = "{length(missing_fields)} metadata field{?s} missing from data."
    ))
  }

  project
}

get_validate_thresholds <- function(sparse_threshold, outlier_iqr_multiplier) {
  if (!is.numeric(sparse_threshold) || sparse_threshold <= 0 || sparse_threshold > 1) {
    cli_abort("{.arg sparse_threshold} must be a number greater than 0 and less than or equal to 1.")
  }

  if (!is.numeric(outlier_iqr_multiplier) || outlier_iqr_multiplier <= 0) {
    cli_abort("{.arg outlier_iqr_multiplier} must be greater than 0.")
  }
}

get_record_id_field_report <- function(data, metadata) {
  metadata_fields <- metadata$field_name[metadata$field_name %in% names(data)]

  if (length(metadata_fields) > 0) {
    return(metadata_fields[[1]])
  }

  non_redcap_fields <- names(data)[!startsWith(names(data), "redcap_")]
  if (length(non_redcap_fields) > 0) {
    return(non_redcap_fields[[1]])
  }

  names(data)[[1]]
}

get_field_summary <- function(project) {
  fields <- project$metadata$field_name[project$metadata$field_name %in% names(project$data)]

  if (length(fields) == 0) {
    return(tibble(
      field_name = character(),
      form_name = character(),
      field_type = character(),
      required_field = logical(),
      record_count = integer(),
      missing_count = integer(),
      observed_count = integer(),
      missing_rate = numeric(),
      distinct_count = integer()
    ))
  }

  bind_rows(map(fields, \(field) {
    metadata_row <- project$metadata |>
      filter(.data$field_name == field) |>
      slice(1)
    field_rows <- get_field_rows(project, metadata_row$form_name)
    value <- project$data[[field]][field_rows]
    missing <- get_is_missing(value)

    tibble(
      field_name = field,
      form_name = metadata_row$form_name,
      field_type = metadata_row$field_type,
      required_field = metadata_row$required_field,
      record_count = length(value),
      missing_count = sum(missing),
      observed_count = sum(!missing),
      missing_rate = if (length(value) == 0) NA_real_ else mean(missing),
      distinct_count = length(unique(value[!missing]))
    )
  }))
}

get_field_rows <- function(project, form_name) {
  if (project$source == "supertbl" && "redcap_form_name" %in% names(project$data)) {
    return(as.character(project$data$redcap_form_name) == as.character(form_name))
  }

  if (project$source == "raw" && "redcap_repeat_instrument" %in% names(project$data)) {
    repeat_instrument <- as.character(project$data$redcap_repeat_instrument)
    repeated_rows <- !get_is_missing(repeat_instrument)
    form_repeat_rows <- repeated_rows & repeat_instrument == as.character(form_name)

    if (any(form_repeat_rows)) {
      completion_field <- paste0(form_name, "_complete")
      if (completion_field %in% names(project$data)) {
        nonrepeat_form_rows <- !repeated_rows & !get_is_missing(project$data[[completion_field]])
        return(form_repeat_rows | nonrepeat_form_rows)
      }

      return(form_repeat_rows)
    }

    return(!repeated_rows)
  }

  rep(TRUE, nrow(project$data))
}

get_missingness_findings <- function(project, field_summary, sparse_threshold) {
  required_fields <- project$metadata |>
    filter(.data$required_field, .data$field_name %in% names(project$data)) |>
    pull(.data$field_name)

  required_findings <- bind_rows(map(required_fields, \(field) {
    metadata_row <- get_metadata_row(project, field)
    field_rows <- get_field_rows(project, metadata_row$form_name)
    missing <- get_is_missing(project$data[[field]]) & field_rows
    if (!any(missing)) {
      return(get_empty_findings())
    }

    tibble(
      check = "missingness",
      issue = "required_field_missing",
      severity = "warning",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][missing]),
      form_name = metadata_row$form_name,
      event_name = get_event_values(project$data, missing),
      field_name = field,
      value = NA_character_,
      expected = "Non-missing value",
      message = paste("Required field", field, "is missing.")
    )
  }))

  sparse_findings <- field_summary |>
    filter(!is.na(.data$missing_rate), .data$missing_rate >= sparse_threshold, .data$record_count > 0) |>
    transmute(
      check = "missingness",
      issue = "unexpected_sparse_field",
      severity = "info",
      scope = "field",
      record_id = NA_character_,
      form_name = .data$form_name,
      event_name = NA_character_,
      field_name = .data$field_name,
      value = as.character(round(.data$missing_rate, 4)),
      expected = paste("Missing rate below", sparse_threshold),
      message = paste("Field", .data$field_name, "is sparse by heuristic threshold.")
    )

  bind_rows(required_findings, sparse_findings)
}

get_metadata_findings <- function(project) {
  metadata <- project$metadata

  duplicate_labels <- metadata |>
    filter(!get_is_missing(.data$field_label)) |>
    group_by(.data$field_label) |>
    summarise(
      field_count = n_distinct(.data$field_name),
      field_name = paste(sort(unique(.data$field_name)), collapse = ", "),
      form_name = paste(sort(unique(.data$form_name)), collapse = ", "),
      .groups = "drop"
    ) |>
    filter(.data$field_count > 1) |>
    transmute(
      check = "metadata",
      issue = "duplicate_field_label",
      severity = "info",
      scope = "field",
      record_id = NA_character_,
      form_name = .data$form_name,
      event_name = NA_character_,
      field_name = .data$field_name,
      value = .data$field_label,
      expected = "Unique field label",
      message = "Multiple fields share the same label."
    )

  missing_labels <- metadata |>
    filter(get_is_missing(.data$field_label)) |>
    transmute(
      check = "metadata",
      issue = "missing_field_label",
      severity = "info",
      scope = "field",
      record_id = NA_character_,
      form_name = .data$form_name,
      event_name = NA_character_,
      field_name = .data$field_name,
      value = NA_character_,
      expected = "Field label",
      message = paste("Field", .data$field_name, "does not have a label.")
    )

  malformed_choices <- metadata |>
    filter(
      .data$field_type %in% c("radio", "dropdown", "checkbox"),
      get_is_missing(.data$select_choices_or_calculations)
    ) |>
    transmute(
      check = "metadata",
      issue = "missing_choice_definition",
      severity = "warning",
      scope = "field",
      record_id = NA_character_,
      form_name = .data$form_name,
      event_name = NA_character_,
      field_name = .data$field_name,
      value = NA_character_,
      expected = "REDCap choice definition",
      message = paste("Choice field", .data$field_name, "does not define choices.")
    )

  branch_orphans <- get_branching_orphans(metadata)
  high_risk_text <- get_high_risk_text_findings(metadata)

  bind_rows(
    duplicate_labels,
    missing_labels,
    malformed_choices,
    branch_orphans,
    high_risk_text
  )
}

get_branching_orphans <- function(metadata) {
  fields <- metadata$field_name
  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    logic <- metadata$branching_logic[[index]]
    refs <- get_branching_references(logic)
    missing_refs <- setdiff(refs, fields)

    if (length(missing_refs) == 0) {
      return(get_empty_findings())
    }

    tibble(
      check = "metadata",
      issue = "orphaned_branching_reference",
      severity = "warning",
      scope = "field",
      record_id = NA_character_,
      form_name = metadata$form_name[[index]],
      event_name = NA_character_,
      field_name = metadata$field_name[[index]],
      value = paste(missing_refs, collapse = ", "),
      expected = "Branching references present in metadata",
      message = paste("Branching logic references missing field(s):", paste(missing_refs, collapse = ", "))
    )
  }))
}

get_high_risk_text_findings <- function(metadata) {
  pattern <- "note|comment|other|describe|free.?text|detail"
  risky <- metadata |>
    filter(
      .data$field_type %in% c("text", "notes"),
      str_detect(tolower(paste(.data$field_name, .data$field_label)), pattern)
    )

  risky |>
    transmute(
      check = "metadata",
      issue = "high_risk_free_text",
      severity = "info",
      scope = "field",
      record_id = NA_character_,
      form_name = .data$form_name,
      event_name = NA_character_,
      field_name = .data$field_name,
      value = .data$field_label,
      expected = "Review free-text PHI risk",
      message = paste("Free-text field", .data$field_name, "may warrant data manager review.")
    )
}

get_outlier_findings <- function(project, outlier_iqr_multiplier) {
  bind_rows(
    get_range_findings(project),
    get_iqr_outlier_findings(project, outlier_iqr_multiplier),
    get_future_date_findings(project)
  )
}

get_range_findings <- function(project) {
  metadata <- project$metadata |>
    filter(
      .data$field_name %in% names(project$data),
      !get_is_missing(.data$text_validation_min) | !get_is_missing(.data$text_validation_max)
    )

  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    field <- metadata$field_name[[index]]
    value <- suppressWarnings(as.numeric(project$data[[field]]))
    min_value <- suppressWarnings(as.numeric(metadata$text_validation_min[[index]]))
    max_value <- suppressWarnings(as.numeric(metadata$text_validation_max[[index]]))
    low <- !is.na(value) & !is.na(min_value) & value < min_value
    high <- !is.na(value) & !is.na(max_value) & value > max_value
    bad <- low | high

    if (!any(bad)) {
      return(get_empty_findings())
    }

    tibble(
      check = "outliers",
      issue = "outside_validation_range",
      severity = "warning",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][bad]),
      form_name = metadata$form_name[[index]],
      event_name = get_event_values(project$data, bad),
      field_name = field,
      value = as.character(project$data[[field]][bad]),
      expected = paste("Between", min_value, "and", max_value),
      message = paste("Field", field, "is outside the REDCap validation range.")
    )
  }))
}

get_iqr_outlier_findings <- function(project, outlier_iqr_multiplier) {
  fields <- project$metadata$field_name[
    project$metadata$field_name %in% names(project$data) &
      project$metadata$field_name != project$record_id_field
  ]

  bind_rows(map(fields, \(field) {
    if (!get_is_numeric_field(project, field)) {
      return(get_empty_findings())
    }

    values <- suppressWarnings(as.numeric(project$data[[field]]))
    observed <- values[!is.na(values)]
    if (length(observed) < 4 || IQR(observed) == 0) {
      return(get_empty_findings())
    }

    quantiles <- quantile(observed, probs = c(0.25, 0.75), names = FALSE)
    iqr <- IQR(observed)
    lower <- quantiles[[1]] - outlier_iqr_multiplier * iqr
    upper <- quantiles[[2]] + outlier_iqr_multiplier * iqr
    outlier <- !is.na(values) & (values < lower | values > upper)

    if (!any(outlier)) {
      return(get_empty_findings())
    }

    metadata_row <- get_metadata_row(project, field)
    tibble(
      check = "outliers",
      issue = "numeric_iqr_outlier",
      severity = "info",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][outlier]),
      form_name = metadata_row$form_name,
      event_name = get_event_values(project$data, outlier),
      field_name = field,
      value = as.character(project$data[[field]][outlier]),
      expected = paste("Between", round(lower, 4), "and", round(upper, 4)),
      message = paste("Field", field, "is outside the IQR heuristic range.")
    )
  }))
}

get_future_date_findings <- function(project) {
  date_fields <- project$metadata |>
    filter(
      .data$field_name %in% names(project$data),
      str_detect(
        tolower(.data$text_validation_type_or_show_slider_number),
        "date"
      )
    )

  bind_rows(map(seq_len(nrow(date_fields)), \(index) {
    field <- date_fields$field_name[[index]]
    value <- as.Date(project$data[[field]])
    future <- !is.na(value) & value > Sys.Date()

    if (!any(future)) {
      return(get_empty_findings())
    }

    tibble(
      check = "outliers",
      issue = "future_date",
      severity = "info",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][future]),
      form_name = date_fields$form_name[[index]],
      event_name = get_event_values(project$data, future),
      field_name = field,
      value = as.character(project$data[[field]][future]),
      expected = paste("On or before", Sys.Date()),
      message = paste("Date field", field, "contains a future date.")
    )
  }))
}

get_operational_findings <- function(project) {
  completion_status <- get_completion_status(project)

  incomplete_forms <- completion_status |>
    filter(
      get_is_missing(.data$value) |
        .data$value != "Complete"
    )

  if (nrow(incomplete_forms) == 0) {
    return(get_empty_findings())
  }

  incomplete_forms |>
    transmute(
      check = "operational",
      issue = "incomplete_form_status",
      severity = "info",
      scope = "record_field",
      record_id = .data$record_id,
      form_name = .data$form_name,
      event_name = .data$event_name,
      field_name = .data$field_name,
      value = .data$value,
      expected = "Complete",
      message = paste("Completion field", .data$field_name, "is not complete.")
    )
}

get_completion_status <- function(project) {
  completion_status <- if (project$source == "supertbl") {
    get_supertbl_completion_status(project)
  } else {
    get_raw_completion_status(project)
  }

  completion_status |>
    mutate(value = get_completion_status_value(.data$value))
}

get_completion_status_value <- function(value) {
  value <- as.character(value)
  value_lower <- tolower(value)

  case_when(
    get_is_missing(value) ~ NA_character_,
    value_lower %in% c("0", "incomplete") ~ "Incomplete",
    value_lower %in% c("1", "unverified") ~ "Unverified",
    value_lower %in% c("2", "complete") ~ "Complete",
    TRUE ~ value
  )
}

get_raw_completion_status <- function(project) {
  completion_fields <- names(project$data)[str_detect(names(project$data), "_complete$")]
  if (length(completion_fields) == 0) {
    return(get_empty_completion_status())
  }

  bind_rows(map(completion_fields, \(field) {
    form_name <- sub("_complete$", "", field)
    form_rows <- get_field_rows(project, form_name)
    if (!any(form_rows)) {
      return(get_empty_completion_status())
    }

    tibble(
      record_id = as.character(project$data[[project$record_id_field]][form_rows]),
      form_name = form_name,
      event_name = get_event_values(project$data, form_rows),
      field_name = field,
      value = as.character(project$data[[field]][form_rows])
    )
  }))
}

get_supertbl_completion_status <- function(project) {
  if (!"form_status_complete" %in% names(project$data)) {
    return(get_empty_completion_status())
  }

  form_name <- as.character(project$data$redcap_form_name)
  observed <- tibble(
    record_id = as.character(project$data[[project$record_id_field]]),
    form_name = form_name,
    event_name = get_event_values(project$data, rep(TRUE, nrow(project$data))),
    field_name = paste0(form_name, "_complete"),
    value = as.character(project$data$form_status_complete)
  )

  expected <- get_expected_supertbl_completion_status(project)
  if (nrow(expected) > 0) {
    nonrepeating_forms <- unique(expected$form_name)
    observed_nonrepeating <- observed |>
      filter(.data$form_name %in% nonrepeating_forms)

    nonrepeating_status <- expected |>
      left_join(
        observed_nonrepeating |>
          rename(.observed_value = "value"),
        by = c("record_id", "event_name", "form_name", "field_name")
      ) |>
      mutate(value = if_else(
        get_is_missing(.data$.observed_value),
        .data$value,
        .data$.observed_value
      )) |>
      select("record_id", "form_name", "event_name", "field_name", "value")

    return(get_ordered_supertbl_completion_status(
      project = project,
      observed = observed,
      expected = nonrepeating_status
    ))
  }

  observed
}

get_ordered_supertbl_completion_status <- function(project, observed, expected) {
  form_order <- get_completion_form_order(project, observed, expected)

  bind_rows(map(form_order, \(form_name) {
    current_form <- form_name
    if (current_form %in% expected$form_name) {
      return(expected |> filter(.data$form_name == current_form))
    }

    observed |> filter(.data$form_name == current_form)
  }))
}

get_completion_form_order <- function(project, observed, expected) {
  structure_forms <- if ("redcap_form_name" %in% names(project$instrument_structure)) {
    as.character(project$instrument_structure$redcap_form_name)
  } else {
    character()
  }

  form_order <- unique(c(
    as.character(project$metadata$form_name),
    structure_forms,
    as.character(expected$form_name),
    as.character(observed$form_name)
  ))

  form_order[!get_is_missing(form_order)]
}

get_expected_supertbl_completion_status <- function(project) {
  nonrepeating_forms <- get_supertbl_nonrepeating_forms(project)
  if (length(nonrepeating_forms) == 0 || nrow(project$data) == 0) {
    return(get_empty_completion_status())
  }

  event_field <- get_event_field(project$data)
  record_event_data <- if (is.na(event_field)) {
    project$data |>
      filter(.data$redcap_form_name %in% nonrepeating_forms)
  } else {
    project$data
  }
  record_events <- tibble(
    record_id = as.character(record_event_data[[project$record_id_field]]),
    event_name = if (is.na(event_field)) {
      NA_character_
    } else {
      as.character(record_event_data[[event_field]])
    }
  ) |>
    distinct() |>
    get_ordered_record_events(project)

  bind_rows(map(nonrepeating_forms, \(form_name) {
    current_form <- form_name
    record_events |>
      mutate(
        form_name = current_form,
        field_name = paste0(current_form, "_complete"),
        value = get_expected_completion_value(project, current_form, .data$event_name)
      )
  }))
}

get_supertbl_nonrepeating_forms <- function(project) {
  forms <- unique(as.character(project$data$redcap_form_name))

  forms[map_lgl(forms, \(form_name) {
    structure <- NA_character_
    if (all(c("redcap_form_name", "structure") %in% names(project$instrument_structure))) {
      structure <- project$instrument_structure |>
        filter(.data$redcap_form_name == form_name) |>
        pull(.data$structure) |>
        first()
    }

    if (!is.na(structure)) {
      return(tolower(structure) != "repeating")
    }

    repeat_instances <- if ("redcap_repeat_instance" %in% names(project$data)) {
      project$data$redcap_repeat_instance[
        as.character(project$data$redcap_form_name) == form_name
      ]
    } else {
      NA_character_
    }
    !any(!get_is_missing(repeat_instances))
  })]
}

get_expected_completion_value <- function(project, form_name, event_name) {
  if (
    nrow(project$events) == 0 ||
      !all(c("redcap_form_name", "redcap_event_name") %in% names(project$events))
  ) {
    return(rep(NA_character_, length(event_name)))
  }

  enabled_events <- project$events |>
    filter(.data$redcap_form_name == form_name) |>
    pull(.data$redcap_event_name) |>
    unique()

  if_else(event_name %in% enabled_events, "Incomplete", NA_character_)
}

get_ordered_record_events <- function(record_events, project) {
  event_order <- get_event_order(project, record_events$event_name)

  record_events |>
    mutate(
      .record_number = suppressWarnings(as.numeric(.data$record_id)),
      .event_order = match(.data$event_name, event_order)
    ) |>
    arrange(
      is.na(.data$.record_number),
      .data$.record_number,
      .data$record_id,
      .data$.event_order,
      .data$event_name
    ) |>
    select(-".record_number", -".event_order")
}

get_event_order <- function(project, event_name) {
  if ("redcap_event_name" %in% names(project$events)) {
    return(unique(as.character(project$events$redcap_event_name)))
  }

  unique(event_name)
}

get_empty_completion_status <- function() {
  tibble(
    record_id = character(),
    form_name = character(),
    event_name = character(),
    field_name = character(),
    value = character()
  )
}

get_consistency_findings <- function(project) {
  checkbox_groups <- split(
    names(project$data)[str_detect(names(project$data), "___")],
    sub("___.*$", "", names(project$data)[str_detect(names(project$data), "___")])
  )

  bind_rows(map(names(checkbox_groups), \(field) {
    columns <- checkbox_groups[[field]]
    none_columns <- columns[str_detect(tolower(columns), "none|not_applicable|n_a|none_of")]

    if (length(none_columns) == 0 || length(setdiff(columns, none_columns)) == 0) {
      return(get_empty_findings())
    }

    none_checked <- Reduce(`|`, lapply(project$data[none_columns], get_is_checked))
    other_checked <- Reduce(`|`, lapply(project$data[setdiff(columns, none_columns)], get_is_checked))
    contradiction <- none_checked & other_checked

    if (!any(contradiction)) {
      return(get_empty_findings())
    }

    metadata_row <- get_metadata_row(project, field)
    tibble(
      check = "consistency",
      issue = "checkbox_none_with_other",
      severity = "warning",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][contradiction]),
      form_name = metadata_row$form_name,
      event_name = get_event_values(project$data, contradiction),
      field_name = field,
      value = paste(none_columns, collapse = ", "),
      expected = "None option not selected with other options",
      message = paste("Checkbox field", field, "has a none option selected with another option.")
    )
  }))
}

get_project_summary <- function(project, findings, field_summary) {
  summary <- tibble(
    source = project$source,
    record_count = n_distinct(project$data[[project$record_id_field]]),
    field_count = n_distinct(project$metadata$field_name),
    form_count = n_distinct(project$metadata$form_name),
    event_count = get_project_event_count(project),
    repeating_enabled = get_repeating_enabled(project),
    mean_missing_rate = if (nrow(field_summary) == 0) NA_real_ else mean(field_summary$missing_rate, na.rm = TRUE),
    finding_count = nrow(findings)
  )

  if (project$source == "raw") {
    summary <- summary |>
      mutate(raw_row_count = nrow(project$data), .after = "record_count")
  } else if (project$source == "supertbl") {
    summary <- summary |>
      mutate(supertbl_row_count = nrow(project$data), .after = "record_count")
  }

  summary
}

get_project_event_count <- function(project) {
  if (nrow(project$events) > 0) {
    event_columns <- intersect(c("redcap_event", "redcap_event_name", "redcap_arm", "arm_name"), names(project$events))
    return(nrow(distinct(project$events, across(all_of(event_columns)))))
  }

  event_field <- get_event_field(project$data)
  if (is.na(event_field)) {
    return(NA_integer_)
  }

  n_distinct(project$data[[event_field]])
}

get_repeating_enabled <- function(project) {
  if (nrow(project$repeating_instruments) > 0) {
    return(TRUE)
  }

  if (
    nrow(project$instrument_structure) > 0 &&
      any(tolower(project$instrument_structure$structure) == "repeating", na.rm = TRUE)
  ) {
    return(TRUE)
  }

  "redcap_repeat_instance" %in% names(project$data)
}

get_form_summary <- function(project, field_summary) {
  if (nrow(field_summary) == 0) {
    return(tibble(
      form_name = character(),
      field_count = integer(),
      required_field_count = integer(),
      mean_missing_rate = numeric()
    ))
  }

  field_summary |>
    group_by(.data$form_name) |>
    summarise(
      field_count = n(),
      required_field_count = sum(.data$required_field),
      mean_missing_rate = mean(.data$missing_rate, na.rm = TRUE),
      .groups = "drop"
    )
}

get_record_summary <- function(project, findings) {
  fields <- project$metadata$field_name[project$metadata$field_name %in% names(project$data)]

  if (length(fields) == 0) {
    return(tibble(
      record_id = character(),
      missing_field_count = integer(),
      finding_count = integer()
    ))
  }

  missing_matrix <- as.data.frame(map(fields, \(field) {
    metadata_row <- get_metadata_row(project, field)
    field_rows <- get_field_rows(project, metadata_row$form_name)
    get_is_missing(project$data[[field]]) & field_rows
  }))
  out <- tibble(
    record_id = as.character(project$data[[project$record_id_field]]),
    missing_field_count = rowSums(missing_matrix)
  )

  finding_counts <- findings |>
    filter(!get_is_missing(.data$record_id)) |>
    count(.data$record_id, name = "finding_count")

  out |>
    left_join(finding_counts, by = "record_id") |>
    mutate(finding_count = if_else(is.na(.data$finding_count), 0L, .data$finding_count))
}

get_event_summary <- function(project) {
  event_field <- get_event_field(project$data)
  if (is.na(event_field)) {
    return(tibble())
  }

  project$data |>
    count(.data[[event_field]], name = "row_count") |>
    rename(event_name = all_of(event_field))
}

get_form_metadata <- function(project) {
  project$metadata |>
    group_by(.data$form_name) |>
    summarise(
      field_count = n(),
      required_field_count = sum(.data$required_field),
      choice_field_count = sum(.data$field_type %in% c("radio", "dropdown", "checkbox")),
      .groups = "drop"
    )
}

get_choice_metadata <- function(project) {
  metadata <- project$metadata |>
    filter(!get_is_missing(.data$select_choices_or_calculations))

  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    choices <- strsplit(metadata$select_choices_or_calculations[[index]], "\\|")[[1]]
    bind_rows(map(choices, \(choice) {
      parts <- strsplit(choice, ",", fixed = TRUE)[[1]]
      tibble(
        field_name = metadata$field_name[[index]],
        form_name = metadata$form_name[[index]],
        choice_value = str_trim(parts[[1]]),
        choice_label = str_trim(paste(parts[-1], collapse = ","))
      )
    }))
  }))
}

get_validation_metadata <- function(project) {
  project$metadata |>
    filter(
      !get_is_missing(.data$text_validation_type_or_show_slider_number) |
        !get_is_missing(.data$text_validation_min) |
        !get_is_missing(.data$text_validation_max)
    ) |>
    select(
      "field_name",
      "form_name",
      "text_validation_type_or_show_slider_number",
      "text_validation_min",
      "text_validation_max"
    )
}

get_branching_metadata <- function(project) {
  project$metadata |>
    filter(!get_is_missing(.data$branching_logic)) |>
    transmute(
      .data$field_name,
      .data$form_name,
      .data$branching_logic,
      referenced_fields = map_chr(.data$branching_logic, \(logic) {
        paste(get_branching_references(logic), collapse = ", ")
      })
    )
}

get_redcapr_data <- function(result, label) {
  if (is.data.frame(result)) {
    return(result)
  }

  if (is.list(result) && "data" %in% names(result)) {
    return(result$data)
  }

  cli_abort("Could not parse REDCapR {label} response.")
}

get_optional_redcapr_data <- function(fun, redcap_uri, token) {
  tryCatch(
    get_redcapr_data(fun(redcap_uri = redcap_uri, token = token), "optional metadata"),
    error = function(cnd) tibble()
  )
}

is_redcap_project_list <- function(x) {
  is.list(x) && !is.data.frame(x) && all(c("data", "metadata") %in% names(x))
}

get_metadata_row <- function(project, field) {
  row <- project$metadata |>
    filter(.data$field_name == field) |>
    slice(1)

  if (nrow(row) == 0) {
    return(tibble(
      field_name = field,
      form_name = NA_character_,
      field_type = NA_character_,
      field_label = NA_character_
    ))
  }

  row
}

get_is_missing <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

get_is_checked <- function(x) {
  tolower(as.character(x)) %in% c("1", "true", "yes", "y", "checked")
}

get_event_values <- function(data, rows) {
  event_field <- get_event_field(data)
  if (is.na(event_field)) {
    return(rep(NA_character_, sum(rows)))
  }

  as.character(data[[event_field]][rows])
}

get_event_field <- function(data) {
  if ("redcap_event_name" %in% names(data)) {
    return("redcap_event_name")
  }

  if ("redcap_event" %in% names(data)) {
    return("redcap_event")
  }

  NA_character_
}

get_is_numeric_field <- function(project, field) {
  value <- project$data[[field]]
  metadata_row <- get_metadata_row(project, field)
  validation <- tolower(metadata_row$text_validation_type_or_show_slider_number)

  is.numeric(value) ||
    validation %in% c("integer", "number", "float", "number_1dp", "number_2dp")
}

get_branching_references <- function(logic) {
  if (length(logic) == 0 || get_is_missing(logic)) {
    return(character())
  }

  matches <- gregexpr("\\[[^]]+\\]", logic, perl = TRUE)
  refs <- regmatches(logic, matches)[[1]]

  if (length(refs) == 0 || refs[[1]] == "-1") {
    return(character())
  }

  refs <- gsub("^\\[|\\]$", "", refs)
  refs <- gsub("\\(.*\\)$", "", refs)
  unique(refs)
}

get_empty_findings <- function() {
  tibble(
    finding_id = integer(),
    check = character(),
    issue = character(),
    severity = character(),
    scope = character(),
    record_id = character(),
    form_name = character(),
    event_name = character(),
    field_name = character(),
    value = character(),
    expected = character(),
    message = character()
  )
}
