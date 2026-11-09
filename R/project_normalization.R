get_quality_project <- function(redcap_uri, token) {
  get_quality_project_data(pull_redcap_project(
    redcap_uri = redcap_uri,
    token = token
  ))
}

get_quality_project_data <- function(project) {
  if (
    !is.list(project) ||
      is.data.frame(project) ||
      !all(c("data", "metadata") %in% names(project))
  ) {
    cli_abort(
      "The REDCap API pull must return {.field data} and {.field metadata} tables."
    )
  }

  out <- list(
    data = get_required_api_table(project$data, "records"),
    metadata = get_standard_metadata(project$metadata),
    events = get_standard_events(project$events),
    event_instruments = get_standard_event_instruments(
      project$event_instruments
    ),
    instruments = get_standard_instruments(project$instruments),
    repeating_instruments = get_standard_repeating_instruments(
      project$repeating_instruments
    ),
    source = "api"
  )

  out <- get_drop_descriptive_fields(out)
  if (nrow(out$data) == 0 && ncol(out$data) == 0) {
    columns <- out$metadata$field_name[out$metadata$field_type != "checkbox"]
    choices <- get_choice_rows(out$metadata)
    checkbox <- choices[choices$choice_type == "checkbox", ]
    checkbox_columns <- if (nrow(checkbox) == 0) {
      character()
    } else {
      paste0(checkbox$field_name, "___", checkbox$choice_value)
    }
    columns <- c(
      columns, checkbox_columns,
      paste0(unique(out$metadata$form_name), "_complete")
    )
    out$data <- as_tibble(setNames(map(unique(columns), \(column) character()), unique(columns)))
  }
  out$record_id_field <- get_record_id_field_report(out$data, out$metadata)
  get_validate_project(out)
}

get_drop_descriptive_fields <- function(project) {
  descriptive_fields <- project$metadata |>
    filter(
      tolower(.data$field_type) %in% c("descriptive", "descriptive_text")
    ) |>
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

  if (
    !"redcap_event_name" %in% names(events) &&
      "unique_event_name" %in% names(events)
  ) {
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

  if (
    !"form_name" %in% names(event_instruments) &&
      "form" %in% names(event_instruments)
  ) {
    event_instruments <- event_instruments |>
      mutate(form_name = as.character(.data$form))
  }

  if (
    !"form_name" %in% names(event_instruments) &&
      "instrument" %in% names(event_instruments)
  ) {
    event_instruments <- event_instruments |>
      mutate(form_name = as.character(.data$instrument))
  }

  if (
    !"redcap_event_name" %in% names(event_instruments) &&
      "unique_event_name" %in% names(event_instruments)
  ) {
    event_instruments <- event_instruments |>
      mutate(redcap_event_name = as.character(.data$unique_event_name))
  }

  if (
    !"redcap_arm" %in% names(event_instruments) &&
      "arm_num" %in% names(event_instruments)
  ) {
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
      field %in%
        names(project$data) ||
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

get_record_id_field_report <- function(data, metadata) {
  if (nrow(metadata) == 0 || get_is_missing(metadata$field_name[[1]])) {
    cli_abort("Metadata must identify the REDCap record ID as its first field.")
  }
  field <- metadata$field_name[[1]]
  if (!field %in% names(data)) {
    cli_abort("Records must include the REDCap record ID field {.field {field}}.")
  }
  field
}

#' @title Standardize instrument names and labels
#' @description Adds canonical instrument columns while retaining source columns.
#' @param instruments REDCap instrument metadata.
#' @returns A tibble with one row per instrument.
#' @noRd
get_standard_instruments <- function(instruments) {
  instruments <- get_optional_api_table(instruments)
  if (nrow(instruments) == 0 && ncol(instruments) == 0) {
    return(tibble(form_name = character(), form_label = character()))
  }

  names(instruments) <- get_clean_names(names(instruments))

  form_column <- intersect(
    c("instrument_name", "form_name", "instrument", "form"),
    names(instruments)
  )
  label_column <- intersect(
    c("instrument_label", "form_label", "label"),
    names(instruments)
  )

  if (length(form_column) == 0) {
    return(tibble(form_name = character(), form_label = character()))
  }

  out <- instruments |>
    mutate(form_name = as.character(.data[[form_column[[1]]]]))

  out$form_label <- if (length(label_column) == 0) {
    NA_character_
  } else {
    as.character(instruments[[label_column[[1]]]])
  }

  out |>
    distinct(.data$form_name, .keep_all = TRUE)
}

#' @title Build ordered form metadata
#' @description Combines dictionary order, instrument labels, and field counts.
#' @param project A normalized REDCap project.
#' @returns A tibble with one row per form.
#' @noRd
get_project_forms <- function(project) {
  forms <- project$metadata |>
    distinct(.data$form_name) |>
    mutate(form_order = row_number())

  if (nrow(project$instruments) > 0) {
    forms <- forms |>
      left_join(project$instruments, by = "form_name")
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
    left_join(
      project$metadata |>
        count(.data$form_name, name = "field_count"),
      by = "form_name"
    ) |>
    select("form_order", "form_name", "form_label", "field_count")
}
