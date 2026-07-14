#' Standardize repeating instrument and event configuration
#'
#' @description
#' Normalizes the metadata returned by
#' [REDCapR::redcap_instrument_repeating()] for internal use.
#'
#' @param repeating_instruments Repeating instrument and event metadata.
#'
#' @returns A tibble containing standardized `form_name` and
#'   `redcap_event_name` columns.
#'
#' @noRd
get_standard_repeating_instruments <- function(repeating_instruments) {
  repeating_instruments <- get_optional_api_table(repeating_instruments)
  if (nrow(repeating_instruments) == 0 && ncol(repeating_instruments) == 0) {
    return(tibble(
      form_name = character(),
      redcap_event_name = character()
    ))
  }

  names(repeating_instruments) <- get_clean_names(names(repeating_instruments))

  if (!"form_name" %in% names(repeating_instruments)) {
    form_column <- intersect(
      c("instrument_name", "redcap_repeat_instrument"),
      names(repeating_instruments)
    )
    repeating_instruments$form_name <- if (length(form_column) == 0) {
      NA_character_
    } else {
      as.character(repeating_instruments[[form_column[[1]]]])
    }
  }

  if (!"redcap_event_name" %in% names(repeating_instruments)) {
    event_column <- intersect(
      c("unique_event_name", "event_name"),
      names(repeating_instruments)
    )
    repeating_instruments$redcap_event_name <- if (length(event_column) == 0) {
      NA_character_
    } else {
      as.character(repeating_instruments[[event_column[[1]]]])
    }
  }

  repeating_instruments |>
    mutate(
      form_name = as.character(.data$form_name),
      redcap_event_name = as.character(.data$redcap_event_name)
    )
}

#' Determine whether a form is configured as a repeating instrument
#'
#' @param project A normalized API project.
#' @param form_name REDCap form name.
#'
#' @returns `TRUE` when `form_name` appears in repeating configuration.
#'
#' @noRd
get_is_repeating_form <- function(project, form_name) {
  any(
    !get_is_missing(project$repeating_instruments$form_name) &
      project$repeating_instruments$form_name == form_name
  )
}

#' Identify exported rows for configured repeating instruments
#'
#' @description
#' Repeating configuration comes from `redcap_instrument_repeating()`. The
#' exported repeat instrument column is used only to associate data rows with
#' that configuration.
#'
#' @inheritParams get_is_repeating_form
#'
#' @returns A logical vector with one value per API data row.
#'
#' @noRd
get_repeating_instrument_rows <- function(project) {
  if (
    !any(!get_is_missing(project$repeating_instruments$form_name)) ||
      !"redcap_repeat_instrument" %in% names(project$data)
  ) {
    return(rep(FALSE, nrow(project$data)))
  }

  !get_is_missing(project$data$redcap_repeat_instrument)
}

#' Identify exported rows for one configured repeating instrument
#'
#' @inheritParams get_is_repeating_form
#'
#' @returns A logical vector with one value per API data row.
#'
#' @noRd
get_matching_repeat_rows <- function(project, form_name) {
  if (!get_is_repeating_form(project, form_name)) {
    return(rep(FALSE, nrow(project$data)))
  }

  repeating_rows <- get_repeating_instrument_rows(project)
  if (!any(repeating_rows)) {
    return(repeating_rows)
  }

  repeating_rows &
    as.character(project$data$redcap_repeat_instrument) == form_name
}
