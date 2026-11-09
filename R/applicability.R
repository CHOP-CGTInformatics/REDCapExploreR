get_field_applicable_rows <- function(project, field) {
  if (
    "field_applicability" %in%
      names(project) &&
      field %in% names(project$field_applicability)
  ) {
    return(project$field_applicability[[field]])
  }

  metadata_row <- get_metadata_row(project, field)

  get_missingness_form_applicable_rows(project, metadata_row$form_name) &
    get_branching_applicable_rows(project, metadata_row$branching_logic)
}

get_field_applicability <- function(project) {
  fields <- get_represented_fields(project)
  if (length(fields) == 0) {
    return(list())
  }

  field_metadata <- project$metadata |>
    filter(.data$field_name %in% fields) |>
    distinct(.data$field_name, .keep_all = TRUE)

  form_rows <- setNames(
    map(unique(field_metadata$form_name), \(form_name) {
      get_missingness_form_applicable_rows(project, form_name)
    }),
    unique(field_metadata$form_name)
  )
  branching_rows <- new.env(parent = emptyenv()) # nolint: object_usage_linter.

  setNames(
    map(fields, \(field) {
      metadata_row <- field_metadata[
        match(field, field_metadata$field_name),
        names(field_metadata),
        drop = FALSE
      ]
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
    }),
    fields
  )
}

get_form_applicable_rows <- function(project, form_name) {
  if ("form_applicability" %in% names(project) && form_name %in% names(project$form_applicability)) {
    return(project$form_applicability[[form_name]])
  }

  get_structural_form_rows(project, form_name) &
    get_form_available_rows(project, form_name)
}

get_form_applicability <- function(project) {
  forms <- unique(project$metadata$form_name)
  setNames(
    map(forms, \(form_name) get_form_applicable_rows(project, form_name)),
    forms
  )
}

get_missingness_form_applicable_rows <- function(project, form_name) {
  get_form_applicable_rows(project, form_name) &
    get_form_completion_assessed_rows(project$data, form_name)
}

get_form_completion_assessed_rows <- function(data, form_name) {
  completion_field <- paste0(form_name, "_complete")
  if (!completion_field %in% names(data)) {
    return(rep(FALSE, nrow(data)))
  }

  get_completion_status_value(data[[completion_field]]) %in%
    c("Unverified", "Complete")
}

get_event_form_enabled_rows <- function(project, form_name) {
  current_form <- form_name
  event_field <- get_event_field(project$data)
  if (
    is.na(event_field) ||
      nrow(project$event_instruments) == 0 ||
      !all(
        c("form_name", "redcap_event_name") %in%
          names(project$event_instruments)
      )
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
  completion_available <- get_form_completion_available_rows(
    project$data,
    form_name
  )
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
