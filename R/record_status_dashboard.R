#' @title Create a Record Status Dashboard dataframe
#'
#' @description
#' `record_status_dashboard()` pulls a REDCap project through the API and
#' returns a plotting-friendly dataframe that mirrors the high-level Record
#' Status Dashboard view.
#'
#' @details
#' The output contains one row per record, event, and form tile that is
#' available from the REDCap export and project metadata. Form completion status
#' values are converted to percentages where `1` means complete, `0` means
#' incomplete or unverified, and `NA` means no completion status was available.
#' Repeating instrument instances are averaged within each record/event/form
#' tile.
#'
#' The record identifier column uses the REDCap project's record ID field name,
#' so a project with `infseq_id` as the record ID field returns an `infseq_id`
#' column. The `form_name` column is ordered for direct use in ggplot displays.
#'
#' @param redcap_uri REDCap API URI.
#' @param token REDCap API token.
#'
#' @returns A tibble with the project record ID field, `form_name`, and
#'   `pct_complete`. Longitudinal projects also include `event_name`.
#'
#' @examples
#' \dontrun{
#' record_status_dashboard(
#'   redcap_uri = Sys.getenv("REDCAP_URI"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#' }
#'
#' @export
record_status_dashboard <- function(redcap_uri, token) {
  if (missing(redcap_uri)) {
    cli_abort("{.arg redcap_uri} and {.arg token} are required.")
  }
  get_validate_api_credentials(redcap_uri, token)

  project <- get_quality_project(
    redcap_uri = redcap_uri,
    token = token
  )

  get_record_status_dashboard(project)
}

get_record_status_dashboard <- function(project) {
  dashboard_grid <- get_dashboard_grid(project)
  completion_summary <- get_dashboard_status_summary(project)

  out <- dashboard_grid |>
    left_join(
      completion_summary,
      by = c("record_id", "event_name", "form_name"),
      relationship = "one-to-one"
    ) |>
    mutate(
      record_id = factor(
        .data$record_id,
        levels = rev(unique(dashboard_grid$record_id))
      ),
      form_name = factor(
        .data$display_form_name,
        levels = unique(dashboard_grid$display_form_name)
      )
    ) |>
    select(
      "record_id",
      any_of("event_name"),
      "form_name",
      "pct_complete"
    )

  if ("event_name" %in% names(out) && all(is.na(out$event_name))) {
    out <- out |>
      select(-"event_name")
  }

  names(out)[names(out) == "record_id"] <- project$record_id_field
  out
}

get_dashboard_grid <- function(project) {
  forms <- get_dashboard_forms(project)
  record_events <- get_dashboard_record_events(project)

  if (nrow(record_events) == 0 || nrow(forms) == 0) {
    return(get_empty_dashboard_grid())
  }

  event_forms <- get_dashboard_event_forms(project, forms)

  if (all(is.na(record_events$event_name))) {
    return(
      get_dashboard_cross_join(record_events, forms) |>
        mutate(display_form_name = .data$form_label) |>
        select("record_id", "event_name", "form_name", "display_form_name")
    )
  }

  record_events |>
    left_join(event_forms, by = "event_name", relationship = "many-to-many") |>
    filter(!is.na(.data$form_name)) |>
    mutate(
      display_form_name = paste(
        .data$event_label,
        .data$form_label,
        sep = " : "
      )
    ) |>
    select("record_id", "event_name", "form_name", "display_form_name")
}

get_dashboard_cross_join <- function(x, y) {
  x |>
    mutate(.join_key = 1L) |>
    left_join(
      y |> mutate(.join_key = 1L),
      by = ".join_key",
      relationship = "many-to-many"
    ) |>
    select(-".join_key")
}

get_empty_dashboard_grid <- function() {
  tibble(
    record_id = character(),
    event_name = character(),
    form_name = character(),
    display_form_name = character()
  )
}

get_dashboard_forms <- function(project) {
  forms <- project$metadata |>
    distinct(.data$form_name) |>
    mutate(form_order = row_number())

  instruments <- get_dashboard_instruments(project)
  if (nrow(instruments) > 0) {
    forms <- forms |>
      left_join(instruments, by = "form_name")
  } else {
    forms$form_label <- NA_character_
  }

  forms |>
    mutate(
      form_label = if_else(
        get_is_missing(.data$form_label),
        .data$form_name,
        .data$form_label
      )
    ) |>
    arrange(.data$form_order) |>
    select("form_name", "form_label", "form_order")
}

get_dashboard_instruments <- function(project) {
  instruments <- get_optional_api_table(project$instruments)
  if (nrow(instruments) == 0 && ncol(instruments) == 0) {
    return(tibble(form_name = character(), form_label = character()))
  }

  names(instruments) <- get_clean_names(names(instruments))

  form_column <- intersect(
    c("instrument_name", "form_name", "form"),
    names(instruments)
  )
  label_column <- intersect(
    c("instrument_label", "form_label", "label"),
    names(instruments)
  )

  if (length(form_column) == 0 || length(label_column) == 0) {
    return(tibble(form_name = character(), form_label = character()))
  }

  instruments |>
    transmute(
      form_name = as.character(.data[[form_column[[1]]]]),
      form_label = as.character(.data[[label_column[[1]]]])
    ) |>
    distinct(.data$form_name, .keep_all = TRUE)
}

get_dashboard_record_events <- function(project) {
  event_field <- get_event_field(project$data)

  base_rows <- get_dashboard_base_row(project$data)

  out <- project$data[base_rows, , drop = FALSE] |>
    transmute(
      record_id = as.character(.data[[project$record_id_field]]),
      event_name = if (is.na(event_field)) NA_character_ else
        as.character(.data[[event_field]])
    ) |>
    distinct(.data$record_id, .data$event_name)

  if (nrow(out) > 0) {
    return(out)
  }

  project$data |>
    transmute(
      record_id = as.character(.data[[project$record_id_field]]),
      event_name = if (is.na(event_field)) NA_character_ else
        as.character(.data[[event_field]])
    ) |>
    distinct(.data$record_id, .data$event_name)
}

get_dashboard_base_row <- function(data) {
  if (!"redcap_repeat_instrument" %in% names(data)) {
    return(rep(TRUE, nrow(data)))
  }

  get_is_missing(data$redcap_repeat_instrument)
}

get_dashboard_event_forms <- function(project, forms) {
  event_field <- get_event_field(project$data)
  if (is.na(event_field)) {
    return(tibble(
      event_name = NA_character_,
      event_label = NA_character_,
      form_name = forms$form_name,
      form_label = forms$form_label
    ))
  }

  event_forms <- get_dashboard_event_form_pairs(project, forms)
  event_labels <- get_dashboard_event_labels(project)

  event_forms |>
    left_join(event_labels, by = "event_name") |>
    left_join(forms, by = "form_name") |>
    mutate(
      event_label = if_else(
        get_is_missing(.data$event_label),
        .data$event_name,
        .data$event_label
      )
    ) |>
    arrange(.data$event_order, .data$form_order) |>
    select("event_name", "event_label", "form_name", "form_label")
}

get_dashboard_event_form_pairs <- function(project, forms) {
  event_instruments <- get_optional_api_table(project$event_instruments)
  if (
    nrow(event_instruments) > 0 &&
      all(c("redcap_event_name", "form_name") %in% names(event_instruments))
  ) {
    return(
      event_instruments |>
        transmute(
          event_name = as.character(.data$redcap_event_name),
          form_name = as.character(.data$form_name)
        ) |>
        filter(.data$form_name %in% forms$form_name) |>
        distinct(.data$event_name, .data$form_name)
    )
  }

  event_field <- get_event_field(project$data)
  events <- project$data |>
    transmute(event_name = as.character(.data[[event_field]])) |>
    distinct(.data$event_name)

  get_dashboard_cross_join(events, forms |> select("form_name"))
}

get_dashboard_event_labels <- function(project) {
  event_field <- get_event_field(project$data)
  data_events <- project$data |>
    transmute(
      event_name = if (is.na(event_field)) NA_character_ else
        as.character(.data[[event_field]])
    ) |>
    distinct(.data$event_name) |>
    mutate(event_order = row_number())

  events <- get_optional_api_table(project$events)
  if (nrow(events) == 0 || !"redcap_event_name" %in% names(events)) {
    return(
      data_events |>
        mutate(event_label = .data$event_name)
    )
  }

  event_label_column <- if ("event_name" %in% names(events)) "event_name" else
    "redcap_event_name"
  events$event_label <- as.character(events[[event_label_column]])

  events |>
    transmute(
      event_name = as.character(.data$redcap_event_name),
      event_label = .data$event_label
    ) |>
    right_join(data_events, by = "event_name") |>
    arrange(.data$event_order)
}

get_dashboard_status_summary <- function(project) {
  completion_status <- get_dashboard_status(project)
  if (nrow(completion_status) == 0) {
    return(tibble(
      record_id = character(),
      event_name = character(),
      form_name = character(),
      pct_complete = numeric()
    ))
  }

  completion_status |>
    mutate(value = get_completion_status_value(.data$value)) |>
    group_by(.data$record_id, .data$event_name, .data$form_name) |>
    summarise(
      pct_complete = get_dashboard_pct_complete(.data$value),
      .groups = "drop"
    )
}

get_dashboard_status <- function(project) {
  completion_fields <- names(project$data)[str_detect(
    names(project$data),
    "_complete$"
  )]
  if (length(completion_fields) == 0) {
    return(get_empty_completion_status())
  }

  bind_rows(map(completion_fields, \(field) {
    form_name <- sub("_complete$", "", field)
    rows <- get_dashboard_completion_rows(project, form_name)

    if (!any(rows)) {
      return(get_empty_completion_status())
    }

    tibble(
      record_id = as.character(project$data[[project$record_id_field]][rows]),
      form_name = form_name,
      event_name = get_event_values(project$data, rows),
      repeat_instrument = get_repeat_instrument_values(project$data, rows),
      repeat_instance = get_repeat_instance_values(project$data, rows),
      field_name = field,
      value = as.character(project$data[[field]][rows])
    )
  }))
}

get_dashboard_completion_rows <- function(project, form_name) {
  repeat_form <- get_is_repeating_form(project, form_name)
  repeated_rows <- get_repeating_instrument_rows(project$data)

  rows <- if (repeat_form) {
    get_matching_repeat_rows(project$data, form_name)
  } else {
    !repeated_rows
  }

  rows & get_event_form_enabled_rows(project, form_name)
}

get_is_repeating_form <- function(project, form_name) {
  if (
    "redcap_repeat_instrument" %in%
      names(project$data) &&
      any(
        as.character(project$data$redcap_repeat_instrument) == form_name,
        na.rm = TRUE
      )
  ) {
    return(TRUE)
  }

  repeating_instruments <- get_optional_api_table(project$repeating_instruments)
  if (nrow(repeating_instruments) == 0) {
    return(FALSE)
  }

  names(repeating_instruments) <- get_clean_names(names(repeating_instruments))

  form_columns <- intersect(
    c("form_name", "instrument_name", "redcap_repeat_instrument"),
    names(repeating_instruments)
  )
  if (length(form_columns) == 0) {
    return(FALSE)
  }

  any(
    as.character(repeating_instruments[[form_columns[[1]]]]) == form_name,
    na.rm = TRUE
  )
}

get_dashboard_pct_complete <- function(value) {
  if (all(get_is_missing(value))) {
    return(NA_real_)
  }

  mean(value == "Complete", na.rm = TRUE)
}
