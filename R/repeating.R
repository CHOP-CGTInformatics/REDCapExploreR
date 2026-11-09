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

#' @title Identify rows belonging to an event and instrument
#'
#' @description Resolves repeating configuration separately for each event.
#' @param project A normalized REDCap project.
#' @param form_name REDCap instrument name.
#' @returns One logical structural membership value per exported row.
#' @noRd
get_structural_form_rows <- function(project, form_name) {
  configuration <- project$repeating_instruments
  configuration <- configuration[
    !get_is_missing(configuration$form_name) & configuration$form_name == form_name, ,
    drop = FALSE
  ]
  events <- get_event_values(project$data, rep(TRUE, nrow(project$data)))
  repeating <- if (all(is.na(events)) || all(get_is_missing(configuration$redcap_event_name))) {
    rep(nrow(configuration) > 0, nrow(project$data))
  } else {
    events %in% configuration$redcap_event_name
  }
  instruments <- get_repeat_instrument_values(project$data, rep(TRUE, nrow(project$data)))
  rows <- (repeating & !get_is_missing(instruments) & instruments == form_name) |
    (!repeating & get_is_missing(instruments))
  rows & get_event_form_enabled_rows(project, form_name)
}
