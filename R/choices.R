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

    bind_rows(map(seq_along(choices), \(choice_index) {
      parts <- strsplit(choices[[choice_index]], ",", fixed = TRUE)[[1]]
      tibble(
        field_order = metadata$field_order[[index]],
        field_name = metadata$field_name[[index]],
        form_name = metadata$form_name[[index]],
        choice_type = tolower(metadata$field_type[[index]]),
        choice_order = choice_index,
        choice_value = str_trim(parts[[1]]),
        choice_label = str_trim(paste(parts[-1], collapse = ","))
      )
    }))
  }))
}
