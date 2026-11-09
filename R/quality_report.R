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
#' @param sparse_threshold Number greater than 0 and less than or equal to 1.
#'   A field is flagged as unexpectedly sparse when its missing rate among
#'   applicable rows is greater than or equal to this value. The default `0.95`
#'   flags fields with at least 95 percent missing values.
#' @param outlier_iqr_multiplier Positive number used to define IQR outlier
#'   bounds as Q1 minus this value times the IQR and Q3 plus this value times the
#'   IQR. Smaller values produce narrower bounds and more findings. The default
#'   is `3`.
#' @param progress Progress display mode. `"auto"` enables normally throttled
#'   progress updates only in interactive sessions. `"none"` suppresses
#'   progress. `"show"` enables progress in all sessions and forces every
#'   update to render.
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
#'   - `high_risk_free_text`
#' - `outliers`
#'   - `outside_validation_range`
#'   - `numeric_iqr_outlier`
#'   - `future_date`
#' - `operational`
#'   - `incomplete_form_status`
#' - `consistency`
#'   - `invalid_choice_value`
#'   - `invalid_validation_format`
#'   - `checkbox_no_values_selected`
#'   - `checkbox_none_with_other`
#'
#' `invalid_validation_format` is evaluated only for text fields with a
#' supported validation configured in
#' `text_validation_type_or_show_slider_number`. Supported validations include
#' REDCap integer and number formats, dates and datetimes, times, email
#' addresses, and North American and Australian phone numbers. Number formats
#' accept decimal values with or without a leading zero. Date and datetime
#' values are validated in REDCap's canonical API export formats, regardless of
#' their configured data-entry display order.
#' `outside_validation_range` enforces the optional `text_validation_min` and
#' `text_validation_max` bounds using the configured validation type.
#' Unvalidated text fields and unsupported validation types are not assessed.
#' `invalid_choice_value` applies to radio, dropdown, Yes/No, and True/False
#' fields; checkbox fields retain their dedicated consistency checks. Choice
#' and text validation checks use structurally applicable form, event, and
#' repeat rows regardless of form completion status or branching logic.
#'
#' @returns A list with `findings`, `summaries`, and `metadata` elements.
#'   `findings` includes record, form, event, repeat instrument, and repeat
#'   instance context when available from the REDCap export.
#'   `summaries$project$raw_row_count` equals `nrow()` for the records returned
#'   by the REDCap API. `summaries$project$field_count` is the count of distinct
#'   standardized metadata field names. `summaries$fields$missing_rate` is the
#'   field-level fraction missing among applicable rows.
#'   `summaries$records` contains one row per record; its missing-field count is
#'   summed across applicable event and repeat rows.
#'   `summaries$forms$missing_rate` and `summaries$project$missing_rate` are
#'   weighted fractions missing across applicable field-row cells. Applicability
#'   accounts for form availability, event-form mapping, repeating instrument
#'   structure, and branching logic. Missingness is only assessed where the raw
#'   REDCap form status column `<form_name>_complete` is `1`/Unverified or
#'   `2`/Complete. Status `0`/Incomplete is handled by operational checks and
#'   does not contribute to missingness. Checkbox parent fields are observed
#'   when their choice exports contain explicit values, including an all-zero
#'   no-selection state; only an all-missing set of choice exports is missing.
#'   `metadata$settings` records the selected checks, thresholds, evaluation date,
#'   and package version. Field `record_count` means assessed rows; record-level
#'   `missing_field_count` counts missing values across events and instances.
#'   Branching expressions that use unsupported REDCap functions or smart
#'   variables are treated as applicable so required values remain reviewable.
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
build_quality_report <- function(
  redcap_uri,
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
  progress = c("auto", "none", "show")
) {
  progress <- match.arg(progress)
  checks <- match.arg(
    checks,
    choices = c(
      "missingness",
      "metadata",
      "outliers",
      "operational",
      "consistency"
    ),
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

  project <- get_quality_project(
    redcap_uri = redcap_uri,
    token = token
  )

  get_quality_report(
    project = project,
    checks = checks,
    sparse_threshold = sparse_threshold,
    outlier_iqr_multiplier = outlier_iqr_multiplier,
    progress_bar = progress_bar,
    progress_enabled = progress_enabled,
    progress_force = progress_force
  )
}

get_quality_report <- function(
  project,
  checks,
  sparse_threshold,
  outlier_iqr_multiplier,
  progress_bar = NULL,
  progress_enabled = FALSE,
  progress_force = FALSE,
  evaluation_date = Sys.Date()
) {
  update_report_progress(
    progress_bar,
    progress_enabled,
    progress_force,
    "Normalized input"
  )

  project$evaluation_date <- evaluation_date
  project$represented_fields <- get_represented_fields(project)
  project$form_applicability <- get_form_applicability(project)
  project$validation_cache <- new.env(parent = emptyenv())
  project$field_value_cache <- new.env(parent = emptyenv())
  project$choices <- get_choice_rows(project$metadata)
  project$field_applicability <- get_field_applicability(project)
  update_report_progress(
    progress_bar,
    progress_enabled,
    progress_force,
    "Built applicability map"
  )

  field_summary <- get_field_summary(project)
  update_report_progress(
    progress_bar,
    progress_enabled,
    progress_force,
    "Built field summaries"
  )

  check_fns <- list(
    missingness = \(...) {
      get_missingness_findings(project, field_summary, sparse_threshold)
    },
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
  update_report_progress(
    progress_bar,
    progress_enabled,
    progress_force,
    "Assembled findings"
  )

  summaries <- list(
    project = get_project_summary(project, findings, field_summary),
    forms = get_form_summary(project, field_summary),
    fields = field_summary,
    records = get_record_summary(project, findings),
    events = get_event_summary(project)
  )
  update_report_progress(
    progress_bar,
    progress_enabled,
    progress_force,
    "Built summaries"
  )

  report_metadata <- list(
    fields = project$metadata,
    forms = get_form_metadata(project),
    events = project$events,
    event_instruments = project$event_instruments,
    instruments = project$instruments,
    repeating_instruments = project$repeating_instruments,
    choices = get_choice_metadata(project),
    validation = get_validation_metadata(project),
    branching = get_branching_metadata(project),
    record_id_field = project$record_id_field,
    source = project$source,
    settings = list(
      checks = checks,
      sparse_threshold = sparse_threshold,
      outlier_iqr_multiplier = outlier_iqr_multiplier,
      evaluation_date = project$evaluation_date,
      package_version = as.character(getNamespaceVersion("REDCapExploreR"))
    )
  )
  update_report_progress(
    progress_bar,
    progress_enabled,
    progress_force,
    "Built metadata"
  )

  report <- list(
    findings = findings,
    summaries = summaries,
    metadata = report_metadata
  )

  class(report) <- c("redcap_quality_report", class(report))
  report
}

#' @export
print.redcap_quality_report <- function(x, ...) {
  project <- x$summaries$project

  cat("<REDCap quality report>\n")
  cat("Records: ", project$record_count, "\n", sep = "")
  cat("Fields: ", project$field_count, "\n", sep = "")
  cat("Findings: ", project$finding_count, "\n", sep = "")
  cat("Checks: ", paste(x$metadata$settings$checks, collapse = ", "), "\n", sep = "")
  assessed <- sum(x$summaries$fields$record_count)
  cat("Missingness assessed: ", assessed, " eligible field-row values\n", sep = "")
  if (assessed == 0) {
    cat("No values were eligible for missingness assessment.\n")
  }
  invisible(x)
}

get_validate_thresholds <- function(sparse_threshold, outlier_iqr_multiplier) {
  if (
    !is.numeric(sparse_threshold) ||
      length(sparse_threshold) != 1 ||
      is.na(sparse_threshold) ||
      !is.finite(sparse_threshold) ||
      sparse_threshold <= 0 ||
      sparse_threshold > 1
  ) {
    cli_abort(
      "{.arg sparse_threshold} must be a number greater than 0 and less than or equal to 1."
    )
  }

  if (
    !is.numeric(outlier_iqr_multiplier) ||
      length(outlier_iqr_multiplier) != 1 ||
      is.na(outlier_iqr_multiplier) ||
      !is.finite(outlier_iqr_multiplier) ||
      outlier_iqr_multiplier <= 0
  ) {
    cli_abort("{.arg outlier_iqr_multiplier} must be greater than 0.")
  }
}

get_field_summary <- function(project) {
  fields <- get_represented_fields(project)

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
    metadata_row <- get_metadata_row(project, field)
    field_rows <- get_field_applicable_rows(project, field)
    value <- get_field_values(project, field)[field_rows]
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

get_represented_fields <- function(project) {
  if ("represented_fields" %in% names(project)) {
    return(project$represented_fields)
  }

  checkbox_fields <- sub(
    "___.*$",
    "",
    names(project$data)[str_detect(names(project$data), "___")]
  )
  represented <- unique(c(names(project$data), checkbox_fields))
  project$metadata$field_name[project$metadata$field_name %in% represented]
}

get_field_values <- function(project, field) {
  if (field %in% names(project$data)) {
    return(project$data[[field]])
  }

  if ("field_value_cache" %in% names(project) && exists(field, envir = project$field_value_cache, inherits = FALSE)) {
    return(project$field_value_cache[[field]])
  }

  checkbox_columns <- names(project$data)[startsWith(
    names(project$data),
    paste0(field, "___")
  )]
  if (length(checkbox_columns) == 0) {
    return(rep(NA_character_, nrow(project$data)))
  }

  checked <- vapply(
    project$data[checkbox_columns],
    get_is_checked,
    logical(nrow(project$data))
  )
  missing <- vapply(
    project$data[checkbox_columns],
    get_is_missing,
    logical(nrow(project$data))
  )
  checked <- matrix(
    checked,
    nrow = nrow(project$data),
    ncol = length(checkbox_columns)
  )
  missing <- matrix(
    missing,
    nrow = nrow(project$data),
    ncol = length(checkbox_columns)
  )

  checked_names <- sub(paste0("^", field, "___"), "", checkbox_columns)
  all_missing <- rowSums(missing) == ncol(missing)
  selected <- !all_missing & rowSums(checked) > 0
  out <- rep("No selections", nrow(project$data))
  out[all_missing] <- NA_character_

  if (any(selected)) {
    keys <- do.call(
      paste0,
      unname(as.data.frame(checked[selected, , drop = FALSE] + 0L))
    )
    unique_keys <- unique(keys)
    first_rows <- which(selected)[match(unique_keys, keys)]
    labels <- map_chr(first_rows, \(row) {
      paste(checked_names[checked[row, ]], collapse = ", ")
    })
    out[selected] <- labels[match(keys, unique_keys)]
  }

  if ("field_value_cache" %in% names(project)) {
    project$field_value_cache[[field]] <- out
  }
  out
}

get_missingness_findings <- function(project, field_summary, sparse_threshold) {
  required_fields <- project$metadata |>
    filter(
      .data$required_field,
      .data$field_name %in% get_represented_fields(project)
    ) |>
    pull(.data$field_name)

  required_findings <- bind_rows(map(required_fields, \(field) {
    metadata_row <- get_metadata_row(project, field)
    field_rows <- get_field_applicable_rows(project, field)
    missing <- get_is_missing(get_field_values(project, field)) & field_rows
    if (!any(missing)) {
      return(get_empty_findings())
    }

    tibble(
      check = "missingness",
      issue = "required_field_missing",
      severity = "warning",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][
        missing
      ]),
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
    filter(
      !is.na(.data$missing_rate),
      .data$missing_rate >= sparse_threshold,
      .data$record_count > 0
    ) |>
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
      message = paste0(
        "Field ", .data$field_name, ": ", .data$missing_count, " of ",
        .data$record_count, " assessed values are missing (",
        round(100 * .data$missing_rate, 1), "%); threshold: ",
        100 * sparse_threshold, "% missing."
      )
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
      message = paste(
        "Choice field",
        .data$field_name,
        "does not define choices."
      )
    )

  high_risk_text <- get_high_risk_text_findings(metadata)

  bind_rows(
    duplicate_labels,
    missing_labels,
    malformed_choices,
    high_risk_text
  )
}

get_high_risk_text_findings <- function(metadata) {
  pattern <- paste0(
    "(?:^|[^a-z])",
    "(?:notes?|comments?|other|describe|free.?text|details?)",
    "(?:[^a-z]|$)"
  )
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
      expected = "Review whether free-text content needs additional safeguards",
      message = paste(
        "Free-text review suggested for", .data$field_name,
        "based on its name or label; stored content was not inspected."
      )
    )
}

get_outlier_findings <- function(project, outlier_iqr_multiplier) {
  today <- project$evaluation_date

  bind_rows(
    get_range_findings(project),
    get_iqr_outlier_findings(project, outlier_iqr_multiplier),
    get_future_date_findings(project, today)
  )
}

get_range_findings <- function(project) {
  metadata <- project$metadata |>
    filter(
      tolower(.data$field_type) == "text",
      .data$field_name %in% names(project$data),
      tolower(.data$text_validation_type_or_show_slider_number) %in%
        get_bounded_text_validations(),
      !get_is_missing(.data$text_validation_min) |
        !get_is_missing(.data$text_validation_max)
    )

  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    field <- metadata$field_name[[index]]
    validation <- tolower(
      metadata$text_validation_type_or_show_slider_number[[index]]
    )
    value <- get_cached_validation_values(project, field, validation)
    min_text <- as.character(metadata$text_validation_min[[index]])
    max_text <- as.character(metadata$text_validation_max[[index]])
    min_value <- get_parsed_validation_values(min_text, validation)
    max_value <- get_parsed_validation_values(max_text, validation)
    applicable <- get_form_applicable_rows(
      project,
      metadata$form_name[[index]]
    )
    low <- applicable & !is.na(value) & !is.na(min_value) & value < min_value
    high <- applicable & !is.na(value) & !is.na(max_value) & value > max_value
    bad <- low | high

    if (!any(bad)) {
      return(get_empty_findings())
    }

    expected <- case_when(
      !get_is_missing(min_text) && !get_is_missing(max_text) ~
        paste(
          "Between",
          min_text,
          "and",
          max_text
        ),
      !get_is_missing(min_text) ~ paste("At least", min_text),
      TRUE ~ paste("At most", max_text)
    )

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
      expected = expected,
      message = paste("Field", field, "is outside the REDCap validation range.")
    )
  }))
}

get_iqr_outlier_findings <- function(project, outlier_iqr_multiplier) {
  fields <- project$metadata$field_name[
    project$metadata$field_name %in%
      names(project$data) &
      project$metadata$field_name != project$record_id_field
  ]

  bind_rows(map(fields, \(field) {
    if (!get_is_numeric_field(project, field)) {
      return(get_empty_findings())
    }

    metadata_row <- get_metadata_row(project, field)
    validation <- tolower(
      metadata_row$text_validation_type_or_show_slider_number
    )
    values <- if (validation %in% get_supported_text_validations() && get_validation_family(validation) == "number") {
      get_cached_validation_values(project, field, validation)
    } else {
      suppressWarnings(as.numeric(project$data[[field]]))
    }
    applicable <- get_form_applicable_rows(project, metadata_row$form_name)
    nonfinite <- applicable & !is.na(values) & !is.finite(values)
    if (any(nonfinite)) {
      cli_warn("Non-finite values in {.field {field}} were excluded from IQR assessment.")
    }
    observed <- values[applicable & is.finite(values)]
    if (length(observed) < 4) {
      return(get_empty_findings())
    }

    quantiles <- quantile(observed, probs = c(0.25, 0.75), names = FALSE)
    iqr <- quantiles[[2]] - quantiles[[1]]
    if (!is.finite(iqr) || iqr <= 0) {
      return(get_empty_findings())
    }
    lower <- quantiles[[1]] - outlier_iqr_multiplier * iqr
    upper <- quantiles[[2]] + outlier_iqr_multiplier * iqr
    outlier <- applicable & is.finite(values) & (values < lower | values > upper)

    if (!any(outlier)) {
      return(get_empty_findings())
    }

    tibble(
      check = "outliers",
      issue = "numeric_iqr_outlier",
      severity = "info",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][
        outlier
      ]),
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

get_future_date_findings <- function(project, today = Sys.Date()) {
  date_fields <- project$metadata |>
    filter(
      tolower(.data$field_type) == "text",
      .data$field_name %in% names(project$data),
      tolower(.data$text_validation_type_or_show_slider_number) %in%
        get_supported_text_validations(),
      get_validation_family(
        tolower(.data$text_validation_type_or_show_slider_number)
      ) %in%
        c("date", "datetime")
    )

  bind_rows(map(seq_len(nrow(date_fields)), \(index) {
    field <- date_fields$field_name[[index]]
    validation <- tolower(
      date_fields$text_validation_type_or_show_slider_number[[index]]
    )
    value <- get_validation_dates_from_parsed(
      get_cached_validation_values(project, field, validation),
      validation
    )
    applicable <- get_form_applicable_rows(
      project,
      date_fields$form_name[[index]]
    )
    future <- applicable & !is.na(value) & value > today

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
      expected = "On or before today",
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
  value_lower <- tolower(value) # nolint: object_usage_linter.

  case_when(
    get_is_missing(value) ~ NA_character_,
    value_lower %in% c("0", "incomplete") ~ "Incomplete",
    value_lower %in% c("1", "unverified") ~ "Unverified",
    value_lower %in% c("2", "complete") ~ "Complete",
    TRUE ~ value
  )
}

get_raw_completion_status <- function(project, dashboard = FALSE) {
  completion_fields <- names(project$data)[str_detect(
    names(project$data),
    "_complete$"
  )]
  if (length(completion_fields) == 0) {
    return(get_empty_completion_status())
  }

  bind_rows(map(completion_fields, \(field) {
    form_name <- sub("_complete$", "", field)
    form_rows <- if (dashboard) {
      get_structural_form_rows(project, form_name)
    } else {
      get_form_applicable_rows(project, form_name)
    }
    if (!any(form_rows)) {
      return(get_empty_completion_status())
    }

    tibble(
      record_id = as.character(project$data[[project$record_id_field]][
        form_rows
      ]),
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
  bind_rows(
    get_invalid_choice_findings(project),
    get_invalid_validation_findings(project),
    get_checkbox_consistency_findings(project)
  )
}

get_checkbox_consistency_findings <- function(project) {
  choices <- get_project_choice_rows(project)
  checkbox_groups <- split(
    names(project$data)[str_detect(names(project$data), "___")],
    sub(
      "___.*$",
      "",
      names(project$data)[str_detect(names(project$data), "___")]
    )
  )

  if (length(checkbox_groups) == 0) {
    return(get_empty_findings())
  }

  bind_rows(map(names(checkbox_groups), \(field) {
    columns <- checkbox_groups[[field]]
    no_selection_finding <- get_checkbox_no_selection_finding(
      project,
      field,
      columns
    )
    none_columns <- get_none_choice_columns(choices, field, columns)
    other_columns <- setdiff(columns, none_columns)

    if (length(none_columns) == 0 || length(other_columns) == 0) {
      return(no_selection_finding)
    }

    none_checked <- Reduce(
      `|`,
      lapply(project$data[none_columns], get_is_checked)
    )
    other_checked <- Reduce(
      `|`,
      lapply(project$data[other_columns], get_is_checked)
    )
    contradiction <- none_checked &
      other_checked &
      get_field_applicable_rows(project, field)

    if (!any(contradiction, na.rm = TRUE)) {
      return(no_selection_finding)
    }

    metadata_row <- get_metadata_row(project, field)
    bind_rows(
      no_selection_finding,
      tibble(
        check = "consistency",
        issue = "checkbox_none_with_other",
        severity = "warning",
        scope = "record_field",
        record_id = as.character(project$data[[project$record_id_field]][
          contradiction
        ]),
        form_name = metadata_row$form_name,
        event_name = get_event_values(project$data, contradiction),
        repeat_instrument = get_repeat_instrument_values(
          project$data,
          contradiction
        ),
        repeat_instance = get_repeat_instance_values(
          project$data,
          contradiction
        ),
        field_name = field,
        value = paste(none_columns, collapse = ", "),
        expected = "None option not selected with other options",
        message = paste(
          "Checkbox field",
          field,
          "has a none option selected with another option."
        )
      )
    )
  }))
}

get_checkbox_no_selection_finding <- function(project, field, columns) {
  assessed <- get_field_applicable_rows(project, field)
  explicit <- as.data.frame(map(
    project$data[columns],
    \(value) !get_is_missing(value)
  ))
  checked <- as.data.frame(map(project$data[columns], get_is_checked))
  assessed_explicit <- assessed & rowSums(explicit) > 0

  if (
    !any(assessed_explicit) ||
      any(rowSums(checked[assessed_explicit, , drop = FALSE]) > 0)
  ) {
    return(get_empty_findings())
  }

  metadata_row <- get_metadata_row(project, field)
  tibble(
    check = "consistency",
    issue = "checkbox_no_values_selected",
    severity = "info",
    scope = "field",
    record_id = NA_character_,
    form_name = metadata_row$form_name,
    event_name = NA_character_,
    field_name = field,
    value = "0 selected options",
    expected = "Confirm that no selections are intentional",
    message = paste(
      "Checkbox field",
      field,
      "has no selected options among assessed rows."
    )
  )
}

get_none_choice_columns <- function(choices, field, columns) {
  none_labels <- c("none", "no", "not applicable", "n/a", "na", "none of the above")
  none_values <- choices |>
    filter(
      .data$field_name == .env$field,
      str_squish(tolower(.data$choice_label)) %in% none_labels
    ) |>
    pull(.data$choice_value)

  metadata_columns <- paste0(field, "___", none_values)
  intersect(columns, metadata_columns)
}

get_project_summary <- function(project, findings, field_summary) {
  summary <- tibble(
    record_count = n_distinct(project$data[[project$record_id_field]]),
    field_count = n_distinct(project$metadata$field_name),
    form_count = n_distinct(project$metadata$form_name),
    event_count = get_project_event_count(project),
    repeating_enabled = get_repeating_enabled(project),
    missing_rate = get_weighted_missing_rate(
      field_summary$missing_count,
      field_summary$record_count
    ),
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
      c(
        "redcap_event_name",
        "unique_event_name",
        "redcap_event",
        "redcap_arm",
        "arm_num",
        "arm_name"
      ),
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
  nrow(project$repeating_instruments) > 0
}

get_form_summary <- function(project, field_summary) {
  if (nrow(field_summary) == 0) {
    return(tibble(
      form_name = character(),
      field_count = integer(),
      required_field_count = integer(),
      missing_rate = numeric()
    ))
  }

  field_summary |>
    group_by(.data$form_name) |>
    summarise(
      field_count = n(),
      required_field_count = sum(.data$required_field),
      missing_rate = get_weighted_missing_rate(
        .data$missing_count,
        .data$record_count
      ),
      .groups = "drop"
    )
}

get_weighted_missing_rate <- function(missing_count, record_count) {
  denominator <- sum(record_count, na.rm = TRUE)
  if (denominator == 0) {
    NA_real_
  } else {
    sum(missing_count, na.rm = TRUE) / denominator
  }
}

get_record_summary <- function(project, findings) {
  fields <- get_represented_fields(project)

  if (length(fields) == 0) {
    return(tibble(
      record_id = character(),
      missing_field_count = integer(),
      finding_count = integer()
    ))
  }

  missing_matrix <- as.data.frame(map(fields, \(field) {
    field_rows <- get_field_applicable_rows(project, field)
    get_is_missing(get_field_values(project, field)) & field_rows
  }))
  out <- tibble(
    record_id = as.character(project$data[[project$record_id_field]]),
    missing_field_count = rowSums(missing_matrix)
  ) |>
    summarise(
      missing_field_count = sum(.data$missing_field_count),
      .by = "record_id"
    )

  finding_counts <- findings |>
    filter(!get_is_missing(.data$record_id)) |>
    count(.data$record_id, name = "finding_count")

  out |>
    left_join(finding_counts, by = "record_id") |>
    mutate(
      finding_count = if_else(
        is.na(.data$finding_count),
        0L,
        .data$finding_count
      )
    )
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
      choice_field_count = sum(
        .data$field_type %in%
          c("radio", "dropdown", "checkbox", "yesno", "truefalse")
      ),
      .groups = "drop"
    )
}

get_choice_metadata <- function(project) {
  get_project_choice_rows(project) |>
    select("field_name", "form_name", "choice_value", "choice_label")
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

get_metadata_row <- function(project, field) {
  index <- match(field, project$metadata$field_name)

  if (is.na(index)) {
    return(tibble(
      field_name = field,
      form_name = NA_character_,
      field_type = NA_character_,
      field_label = NA_character_
    ))
  }

  project$metadata[index, , drop = FALSE]
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

get_is_numeric_field <- function(project, field) {
  value <- project$data[[field]]
  metadata_row <- get_metadata_row(project, field)
  validation <- tolower(metadata_row$text_validation_type_or_show_slider_number)
  field_type <- tolower(metadata_row$field_type)

  if (field_type %in% c("checkbox", "dropdown", "radio", "truefalse", "yesno")) {
    return(FALSE)
  }

  observed <- as.character(value[!get_is_missing(value)])
  values_are_numeric <- length(observed) > 0 &&
    all(
      !is.na(
        suppressWarnings(as.numeric(observed))
      )
    )

  is.numeric(value) ||
    values_are_numeric ||
    validation %in% "float" ||
    (validation %in% get_supported_text_validations() && get_validation_family(validation) == "number")
}

get_empty_findings <- function() {
  tibble(
    finding_id = integer(),
    !!!setNames(
      rep(list(character()), length(get_finding_columns())),
      get_finding_columns()
    )
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
