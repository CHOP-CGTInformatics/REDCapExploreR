#' @title Build a REDCap data quality report
#'
#' @description
#' `build_quality_report()` pulls a REDCap project through the API, applies
#' general data quality heuristics, and returns findings, summaries, and
#' interpreted metadata.
#'
#' @param redcap_uri REDCap API URI.
#' @param token REDCap API token.
#' @param checks Character vector of check groups to run.
#' @param sparse_threshold Missingness threshold used to flag unexpectedly sparse
#'   fields.
#' @param outlier_iqr_multiplier Multiplier used for IQR-based numeric outlier
#'   detection.
#' @param progress Progress display mode. Use `"auto"` to show progress only in
#'   interactive sessions, `"none"` to suppress progress, or `"show"` to force
#'   progress output.
#'
#' @details
#' Current `findings$issue` values by `findings$check`:
#'
#' - `missingness`
#'   - `required_field_missing`
#'   - `unexpected_sparse_field`
#' - `metadata`
#'   - `duplicate_field_label`
#'   - `missing_field_label`
#'   - `missing_choice_definition`
#'   - `orphaned_branching_reference`
#'   - `high_risk_free_text`
#' - `outliers`
#'   - `outside_validation_range`
#'   - `numeric_iqr_outlier`
#'   - `future_date`
#' - `operational`
#'   - `incomplete_form_status`
#' - `consistency`
#'   - `checkbox_none_with_other`
#'
#' @returns A list with `findings`, `summaries`, and `metadata` elements.
#'   `findings` includes record, form, event, repeat instrument, and repeat
#'   instance context when available from the REDCap export.
#'   `summaries$project$raw_row_count` equals `nrow()` for the records returned
#'   by the REDCap API. `summaries$project$field_count` is the count of distinct
#'   standardized metadata field names.
#'
#' @examples
#' \dontrun{
#' report <- build_quality_report(
#'   redcap_uri = Sys.getenv("REDCAP_URI"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#' }
#'
#' @export
build_quality_report <- function(redcap_uri,
                                 token,
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
  if (missing(redcap_uri)) {
    cli_abort("{.arg redcap_uri} and {.arg token} are required.")
  }
  get_validate_api_credentials(redcap_uri, token)

  project <- get_quality_project(
    redcap_uri = redcap_uri,
    token = token
  )
  update_report_progress(progress_bar, progress_enabled, progress_force, "Normalized input")

  project <- get_project_field_applicability(project)
  update_report_progress(progress_bar, progress_enabled, progress_force, "Built applicability map")

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
      get_standard_findings() |>
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
    event_instruments = project$event_instruments,
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
#' @returns A list containing raw records, metadata, events, event-instrument
#'   mapping, instruments, and repeating instrument configuration.
#'
#' @export
pull_redcap_project <- function(redcap_uri, token) {
  get_validate_api_credentials(redcap_uri, token)

  data_result <- redcap_read_oneshot(
    redcap_uri = redcap_uri,
    token = token,
    verbose = FALSE
  )
  metadata_result <- redcap_metadata_read(
    redcap_uri = redcap_uri,
    token = token,
    verbose = FALSE
  )

  list(
    data = get_redcapr_data(data_result, "records"),
    metadata = get_redcapr_data(metadata_result, "metadata"),
    events = get_optional_redcapr_data(redcap_event_read, redcap_uri, token),
    event_instruments = get_optional_redcapr_data(redcap_event_instruments, redcap_uri, token),
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
  length(checks) + 6L
}

get_validate_api_credentials <- function(redcap_uri, token) {
  invalid_credentials <- missing(redcap_uri) ||
    missing(token) ||
    length(redcap_uri) != 1 ||
    length(token) != 1 ||
    is.na(redcap_uri) ||
    is.na(token) ||
    redcap_uri == "" ||
    token == ""

  if (invalid_credentials) {
    cli_abort("{.arg redcap_uri} and {.arg token} are required.")
  }

  invisible(NULL)
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

get_quality_project <- function(redcap_uri, token) {
  out <- get_api_project(pull_redcap_project(
    redcap_uri = redcap_uri,
    token = token
  ))

  out$record_id_field <- get_record_id_field_report(out$data, out$metadata)
  out
}

get_api_project <- function(project) {
  if (!is.list(project) || is.data.frame(project) || !all(c("data", "metadata") %in% names(project))) {
    cli_abort("The REDCap API pull must return {.field data} and {.field metadata} tables.")
  }

  out <- list(
    data = get_required_api_table(project$data, "records"),
    metadata = get_standard_metadata(project$metadata),
    events = get_standard_events(project$events),
    event_instruments = get_standard_event_instruments(project$event_instruments),
    instruments = get_optional_api_table(project$instruments),
    repeating_instruments = get_optional_api_table(project$repeating_instruments),
    instrument_structure = tibble(),
    source = "api"
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

get_required_api_table <- function(x, label) {
  if (is.data.frame(x)) {
    return(as_tibble(x))
  }

  cli_abort("The REDCap API {label} response must be a data frame.")
}

get_optional_api_table <- function(x) {
  if (is.null(x)) {
    return(tibble())
  }

  if (is.data.frame(x)) {
    return(as_tibble(x))
  }

  cli_abort("Optional REDCap API metadata must be a data frame.")
}

get_standard_events <- function(events) {
  events <- get_optional_api_table(events)
  if (nrow(events) == 0 && ncol(events) == 0) {
    return(events)
  }

  names(events) <- get_clean_names(names(events))

  if (!"redcap_event_name" %in% names(events) && "unique_event_name" %in% names(events)) {
    events <- events |>
      mutate(redcap_event_name = as.character(.data$unique_event_name))
  }

  if (!"redcap_arm" %in% names(events) && "arm_num" %in% names(events)) {
    events <- events |>
      mutate(redcap_arm = as.character(.data$arm_num))
  }

  events
}

get_standard_event_instruments <- function(event_instruments) {
  event_instruments <- get_optional_api_table(event_instruments)
  if (nrow(event_instruments) == 0 && ncol(event_instruments) == 0) {
    return(event_instruments)
  }

  names(event_instruments) <- get_clean_names(names(event_instruments))

  if (!"form_name" %in% names(event_instruments) && "form" %in% names(event_instruments)) {
    event_instruments <- event_instruments |>
      mutate(form_name = as.character(.data$form))
  }

  if (!"redcap_event_name" %in% names(event_instruments) && "unique_event_name" %in% names(event_instruments)) {
    event_instruments <- event_instruments |>
      mutate(redcap_event_name = as.character(.data$unique_event_name))
  }

  if (!"redcap_arm" %in% names(event_instruments) && "arm_num" %in% names(event_instruments)) {
    event_instruments <- event_instruments |>
      mutate(redcap_arm = as.character(.data$arm_num))
  }

  event_instruments
}

get_standard_metadata <- function(metadata) {
  metadata <- get_required_api_table(metadata, "metadata")
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
    field_rows <- get_field_applicable_rows(project, field)
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
  get_form_applicable_rows(project, form_name)
}

get_field_applicable_rows <- function(project, field) {
  if ("field_applicability" %in% names(project) && field %in% names(project$field_applicability)) {
    return(project$field_applicability[[field]])
  }

  metadata_row <- get_metadata_row(project, field)

  get_form_applicable_rows(project, metadata_row$form_name) &
    get_branching_applicable_rows(project, metadata_row$branching_logic)
}

get_field_applicability <- function(project) {
  fields <- project$metadata$field_name[project$metadata$field_name %in% names(project$data)]
  if (length(fields) == 0) {
    return(list())
  }

  field_metadata <- project$metadata |>
    filter(.data$field_name %in% fields) |>
    distinct(.data$field_name, .keep_all = TRUE)

  form_rows <- setNames(
    map(unique(field_metadata$form_name), \(form_name) {
      get_form_applicable_rows(project, form_name)
    }),
    unique(field_metadata$form_name)
  )
  branching_rows <- new.env(parent = emptyenv())

  setNames(map(fields, \(field) {
    metadata_row <- field_metadata |>
      filter(.data$field_name == field) |>
      slice(1)
    logic_key <- as.character(metadata_row$branching_logic)
    if (get_is_missing(logic_key)) {
      logic_key <- "__missing_branching_logic__"
    }

    if (!exists(logic_key, envir = branching_rows, inherits = FALSE)) {
      branching_rows[[logic_key]] <- get_branching_applicable_rows(
        project = project,
        logic = metadata_row$branching_logic
      )
    }

    form_rows[[metadata_row$form_name]] & branching_rows[[logic_key]]
  }), fields)
}

get_project_field_applicability <- function(project) {
  project$field_applicability <- get_field_applicability(project)
  project
}

get_form_applicable_rows <- function(project, form_name) {
  repeated_rows <- get_repeating_instrument_rows(project$data)
  matching_repeat_rows <- get_matching_repeat_rows(project$data, form_name)
  nonrepeat_rows <- !repeated_rows

  matching_repeat_rows |
    (
      nonrepeat_rows &
        get_event_form_enabled_rows(project, form_name) &
        get_form_available_rows(project, form_name)
    )
}

get_repeating_instrument_rows <- function(data) {
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }

  !get_is_missing(data$redcap_repeat_instrument)
}

get_matching_repeat_rows <- function(data, form_name) {
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }

  !get_is_missing(data$redcap_repeat_instrument) &
    as.character(data$redcap_repeat_instrument) == as.character(form_name)
}

get_event_form_enabled_rows <- function(project, form_name) {
  current_form <- form_name
  event_field <- get_event_field(project$data)
  if (
    is.na(event_field) ||
      nrow(project$event_instruments) == 0 ||
      !all(c("form_name", "redcap_event_name") %in% names(project$event_instruments))
  ) {
    return(rep(TRUE, nrow(project$data)))
  }

  enabled_events <- project$event_instruments |>
    filter(.data$form_name == .env$current_form) |>
    pull(.data$redcap_event_name) |>
    unique()

  as.character(project$data[[event_field]]) %in% enabled_events
}

get_form_available_rows <- function(project, form_name) {
  completion_available <- get_form_completion_available_rows(project$data, form_name)
  field_columns <- get_form_data_columns(project, form_name)

  if (length(field_columns) == 0) {
    return(completion_available)
  }

  field_available <- Reduce(
    `|`,
    lapply(project$data[field_columns], \(value) !get_is_missing(value))
  )

  completion_available | field_available
}

get_form_completion_available_rows <- function(data, form_name) {
  completion_field <- paste0(form_name, "_complete")
  if (!completion_field %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }

  !get_is_missing(data[[completion_field]])
}

get_form_data_columns <- function(project, form_name) {
  current_form <- form_name
  fields <- project$metadata |>
    filter(
      .data$form_name == .env$current_form,
      .data$field_name != project$record_id_field
    ) |>
    pull(.data$field_name)

  unique(flatten_chr(map(fields, \(field) {
    if (field %in% names(project$data)) {
      return(field)
    }

    names(project$data)[startsWith(names(project$data), paste0(field, "___"))]
  })))
}

get_missingness_findings <- function(project, field_summary, sparse_threshold) {
  required_fields <- project$metadata |>
    filter(.data$required_field, .data$field_name %in% names(project$data)) |>
    pull(.data$field_name)

  required_findings <- bind_rows(map(required_fields, \(field) {
    metadata_row <- get_metadata_row(project, field)
    field_rows <- get_field_applicable_rows(project, field)
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
      repeat_instrument = get_repeat_instrument_values(project$data, missing),
      repeat_instance = get_repeat_instance_values(project$data, missing),
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
      repeat_instrument = get_repeat_instrument_values(project$data, bad),
      repeat_instance = get_repeat_instance_values(project$data, bad),
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
      repeat_instrument = get_repeat_instrument_values(project$data, outlier),
      repeat_instance = get_repeat_instance_values(project$data, outlier),
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
      repeat_instrument = get_repeat_instrument_values(project$data, future),
      repeat_instance = get_repeat_instance_values(project$data, future),
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
      repeat_instrument = .data$repeat_instrument,
      repeat_instance = .data$repeat_instance,
      field_name = .data$field_name,
      value = .data$value,
      expected = "Complete",
      message = paste("Completion field", .data$field_name, "is not complete.")
    )
}

get_completion_status <- function(project) {
  get_raw_completion_status(project) |>
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
      repeat_instrument = get_repeat_instrument_values(project$data, form_rows),
      repeat_instance = get_repeat_instance_values(project$data, form_rows),
      field_name = field,
      value = as.character(project$data[[field]][form_rows])
    )
  }))
}

get_empty_completion_status <- function() {
  tibble(
    record_id = character(),
    form_name = character(),
    event_name = character(),
    repeat_instrument = character(),
    repeat_instance = character(),
    field_name = character(),
    value = character()
  )
}

get_consistency_findings <- function(project) {
  checkbox_groups <- split(
    names(project$data)[str_detect(names(project$data), "___")],
    sub("___.*$", "", names(project$data)[str_detect(names(project$data), "___")])
  )

  if (length(checkbox_groups) == 0) {
    return(get_empty_findings())
  }

  bind_rows(map(names(checkbox_groups), \(field) {
    columns <- checkbox_groups[[field]]
    none_columns <- columns[str_detect(columns, "___(none|no|not_applicable|na)$")]
    other_columns <- setdiff(columns, none_columns)

    if (length(none_columns) == 0 || length(other_columns) == 0) {
      return(get_empty_findings())
    }

    none_checked <- Reduce(`|`, lapply(project$data[none_columns], get_is_checked))
    other_checked <- Reduce(`|`, lapply(project$data[other_columns], get_is_checked))
    contradiction <- none_checked & other_checked

    if (!any(contradiction, na.rm = TRUE)) {
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
      repeat_instrument = get_repeat_instrument_values(project$data, contradiction),
      repeat_instance = get_repeat_instance_values(project$data, contradiction),
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

  summary |>
    mutate(raw_row_count = nrow(project$data), .after = "record_count")
}

get_project_event_count <- function(project) {
  event_field <- get_event_field(project$data)
  if (!is.na(event_field)) {
    return(n_distinct(project$data[[event_field]]))
  }

  if (nrow(project$events) > 0) {
    event_columns <- intersect(
      c("redcap_event_name", "unique_event_name", "redcap_event", "redcap_arm", "arm_num", "arm_name"),
      names(project$events)
    )

    if (length(event_columns) == 0) {
      return(nrow(project$events))
    }

    return(nrow(distinct(project$events, across(all_of(event_columns)))))
  }

  NA_integer_
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
    field_rows <- get_field_applicable_rows(project, field)
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
    get_redcapr_data(
      fun(redcap_uri = redcap_uri, token = token, verbose = FALSE),
      "optional metadata"
    ),
    error = function(cnd) tibble()
  )
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

get_repeat_instrument_values <- function(data, rows) {
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(rep(NA_character_, sum(rows)))
  }

  as.character(data$redcap_repeat_instrument[rows])
}

get_repeat_instance_values <- function(data, rows) {
  if (!"redcap_repeat_instance" %in% names(data)) {
    return(rep(NA_character_, sum(rows)))
  }

  as.character(data$redcap_repeat_instance[rows])
}

get_branching_applicable_rows <- function(project,
                                          logic,
                                          branching_environment = NULL) {
  if (length(logic) == 0 || get_is_missing(logic)) {
    return(rep(TRUE, nrow(project$data)))
  }

  out <- tryCatch(
    get_evaluated_branching_logic(project$data, logic, branching_environment),
    error = function(cnd) rep(TRUE, nrow(project$data))
  )

  if (!is.logical(out) || length(out) != nrow(project$data)) {
    return(rep(TRUE, nrow(project$data)))
  }

  out[is.na(out)] <- FALSE
  out
}

get_evaluated_branching_logic <- function(data, logic, branching_environment = NULL) {
  expression <- get_branching_expression(data, logic)
  parsed <- parse(text = expression)

  if (length(parsed) != 1) {
    return(rep(TRUE, nrow(data)))
  }

  if (is.null(branching_environment)) {
    branching_environment <- get_branching_environment(
      data,
      fields = get_branching_expression_references(expression)
    )
  }

  out <- eval(parsed[[1]], envir = branching_environment)
  as.logical(out)
}

get_branching_expression <- function(data, logic) {
  expression <- as.character(logic)
  expression <- gsub("<>", "!=", expression, fixed = TRUE)
  expression <- gsub("(?i)\\band\\b", "&", expression, perl = TRUE)
  expression <- gsub("(?i)\\bor\\b", "|", expression, perl = TRUE)
  expression <- gsub("(?<![<>=!])=(?!=)", "==", expression, perl = TRUE)
  expression <- gsub("\\[(\\w+)\\(([^)]+)\\)\\]", "`.\\1___\\2`", expression, perl = TRUE)
  expression <- gsub("\\[([^]]+)\\]", "`.\\1`", expression, perl = TRUE)

  referenced_values <- gregexpr("`\\.([^`]+)`", expression, perl = TRUE)
  references <- regmatches(expression, referenced_values)[[1]]
  reference_names <- gsub("^`\\.|`$", "", references)
  if (length(reference_names) > 0 && any(!reference_names %in% names(data))) {
    return("TRUE")
  }

  expression
}

get_branching_expression_references <- function(expression) {
  referenced_values <- gregexpr("`\\.([^`]+)`", expression, perl = TRUE)
  references <- regmatches(expression, referenced_values)[[1]]
  unique(gsub("^`\\.|`$", "", references))
}

get_branching_environment <- function(data, fields = names(data)) {
  env <- new.env(parent = emptyenv())
  env$`&` <- `&`
  env$`|` <- `|`
  env$`!` <- `!`
  env$`==` <- `==`
  env$`!=` <- `!=`
  env$`<` <- `<`
  env$`<=` <- `<=`
  env$`>` <- `>`
  env$`>=` <- `>=`
  env$`(` <- `(`
  env$`c` <- c

  map(intersect(fields, names(data)), \(field) {
    env[[paste0(".", field)]] <- get_branching_value(data[[field]])
    NULL
  })

  env
}

get_branching_value <- function(value) {
  value <- as.character(value)
  value[get_is_missing(value)] <- NA_character_
  value
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
    !!!setNames(rep(list(character()), length(get_finding_columns())), get_finding_columns())
  )
}

get_standard_findings <- function(findings) {
  missing_columns <- setdiff(get_finding_columns(), names(findings))
  for (column in missing_columns) {
    findings[[column]] <- NA_character_
  }

  findings |>
    select(all_of(get_finding_columns()))
}

get_finding_columns <- function() {
  c(
    "check",
    "issue",
    "severity",
    "scope",
    "record_id",
    "form_name",
    "event_name",
    "repeat_instrument",
    "repeat_instance",
    "field_name",
    "value",
    "expected",
    "message"
  )
}
