#' Parse REDCap choices
#'
#' @description
#' Converts explicit and implicit choices for choice-capable fields into one row
#' per coded value. Calculated fields and other uses of the shared REDCap
#' metadata column are excluded.
#'
#' @param metadata Standardized REDCap field metadata.
#'
#' @returns A tibble with field, form, choice type, order, value, and label
#'   columns.
#'
#' @noRd
get_choice_rows <- function(metadata) {
  if (!"field_order" %in% names(metadata)) {
    metadata$field_order <- NA_integer_
  }

  metadata <- metadata |>
    mutate(
      choice_definition = case_when(
        tolower(.data$field_type) == "yesno" ~ "1, Yes | 0, No",
        tolower(.data$field_type) == "truefalse" ~ "1, True | 0, False",
        TRUE ~ as.character(.data$select_choices_or_calculations)
      )
    ) |>
    filter(
      tolower(.data$field_type) %in%
        c("checkbox", "dropdown", "radio", "yesno", "truefalse"),
      !get_is_missing(.data$choice_definition)
    )

  if (nrow(metadata) == 0) {
    return(tibble(
      field_order = integer(),
      field_name = character(),
      form_name = character(),
      choice_type = character(),
      choice_order = integer(),
      choice_value = character(),
      choice_label = character()
    ))
  }

  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    choices <- strsplit(
      metadata$choice_definition[[index]],
      "\\|"
    )[[1]]
    parts <- strsplit(choices, ",", fixed = TRUE)
    tibble(
      field_order = metadata$field_order[[index]],
      field_name = metadata$field_name[[index]],
      form_name = metadata$form_name[[index]],
      choice_type = tolower(metadata$field_type[[index]]),
      choice_order = seq_along(choices),
      choice_value = map_chr(parts, \(part) str_trim(part[[1]])),
      choice_label = map_chr(parts, \(part) {
        str_trim(paste(part[-1], collapse = ","))
      })
    )
  }))
}

get_project_choice_rows <- function(project) {
  if ("choices" %in% names(project)) {
    return(project$choices)
  }

  get_choice_rows(project$metadata)
}

get_invalid_choice_findings <- function(project) {
  choices <- get_project_choice_rows(project)
  fields <- choices |>
    filter(.data$choice_type != "checkbox") |>
    distinct(
      .data$field_name,
      .data$form_name,
      .data$choice_type
    )

  bind_rows(map(seq_len(nrow(fields)), \(index) {
    field <- fields$field_name[[index]]
    allowed <- choices |>
      filter(.data$field_name == .env$field) |>
      pull(.data$choice_value)

    if (!field %in% names(project$data)) {
      return(get_empty_findings())
    }

    value <- as.character(project$data[[field]])
    applicable <- get_form_applicable_rows(
      project,
      fields$form_name[[index]]
    )
    invalid <- applicable & !get_is_missing(value) & !value %in% allowed
    get_invalid_choice_finding_rows(
      project,
      field,
      fields$form_name[[index]],
      invalid,
      value[invalid],
      allowed
    )
  }))
}

get_invalid_choice_finding_rows <- function(
  project,
  field,
  form_name,
  invalid,
  value,
  allowed
) {
  if (!any(invalid)) {
    return(get_empty_findings())
  }

  tibble(
    check = "consistency",
    issue = "invalid_choice_value",
    severity = "warning",
    scope = "record_field",
    record_id = as.character(project$data[[project$record_id_field]][invalid]),
    form_name = form_name,
    event_name = get_event_values(project$data, invalid),
    repeat_instrument = get_repeat_instrument_values(project$data, invalid),
    repeat_instance = get_repeat_instance_values(project$data, invalid),
    field_name = field,
    value = value,
    expected = paste("One of:", paste(allowed, collapse = ", ")),
    message = paste("Field", field, "contains an invalid REDCap choice value.")
  )
}
