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
#'   Errors from any structural metadata endpoint are reported rather than
#'   converted into empty project structure. Records and headers use raw REDCap
#'   codes with automatic type guessing disabled, and checkbox choices are
#'   returned as raw `0`/`1` values. Only empty strings are imported as missing.
#'   Gray form statuses are exported as blanks rather than Incomplete (`0`).
#'
#' @export
pull_redcap_project <- function(redcap_uri, token) {
  get_validate_api_credentials(redcap_uri, token)

  list(
    data = pull_redcap_table(
      redcap_read_oneshot, redcap_uri, token, "records",
      raw_or_label = "raw", raw_or_label_headers = "raw",
      export_checkbox_label = FALSE, na = "", guess_type = FALSE,
      blank_for_gray_form_status = TRUE
    ),
    metadata = pull_redcap_table(redcap_metadata_read, redcap_uri, token, "metadata"),
    events = pull_redcap_table(
      redcap_event_read,
      redcap_uri,
      token,
      "event metadata"
    ),
    event_instruments = pull_redcap_table(
      redcap_event_instruments,
      redcap_uri,
      token,
      "event-instrument metadata"
    ),
    instruments = pull_redcap_table(
      redcap_instruments,
      redcap_uri,
      token,
      "instrument metadata"
    ),
    repeating_instruments = pull_redcap_table(
      redcap_instrument_repeating,
      redcap_uri,
      token,
      "repeating-instrument metadata"
    )
  )
}

get_validate_api_credentials <- function(redcap_uri, token) {
  invalid_credentials <- missing(redcap_uri) ||
    missing(token) ||
    !is.character(redcap_uri) ||
    !is.character(token) ||
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

get_redcapr_data <- function(result, label) {
  if (is.data.frame(result)) {
    return(as_tibble(result))
  }
  if (is.list(result) && "data" %in% names(result)) {
    if ("success" %in% names(result) && !identical(result$success, TRUE)) {
      # REDCapR returns this exact response for a valid classic project.
      classic <- label == "event metadata" &&
        identical(result$raw_text, "ERROR: You cannot export events for classic projects")
      if (classic) {
        return(tibble())
      }
      diagnostic <- if (is.character(result$outcome_message) && length(result$outcome_message) == 1) {
        result$outcome_message
      } else {
        "No outcome message was returned."
      }
      cli_abort("Could not retrieve REDCap {label}: the API operation was unsuccessful. {diagnostic}")
    }
    return(get_required_api_table(result$data, label))
  }
  cli_abort("Could not parse REDCapR {label} response.")
}

#' @title Retrieve and validate one REDCap API table
#' @description Adds endpoint context and removes credentials from errors.
#' @param fun REDCapR retrieval function.
#' @param redcap_uri REDCap API URI.
#' @param token REDCap API token.
#' @param label Human-readable endpoint name.
#' @param ... Additional REDCapR arguments.
#' @returns The retrieved table.
#' @noRd
pull_redcap_table <- function(fun, redcap_uri, token, label, ...) {
  tryCatch(
    get_redcapr_data(
      fun(redcap_uri = redcap_uri, token = token, verbose = FALSE, ...), label
    ),
    error = function(cnd) {
      diagnostic <- conditionMessage(cnd)
      diagnostic <- gsub(token, "[redacted token]", diagnostic, fixed = TRUE)
      diagnostic <- gsub(redcap_uri, "[REDCap endpoint]", diagnostic, fixed = TRUE)
      cli_abort("Could not retrieve REDCap {label}.", parent = simpleError(diagnostic))
    }
  )
}

pull_redcap_codebook <- function(redcap_uri, token) {
  get_validate_api_credentials(redcap_uri, token)

  list(
    metadata = pull_redcap_table(redcap_metadata_read, redcap_uri, token, "metadata"),
    events = pull_redcap_table(
      redcap_event_read,
      redcap_uri,
      token,
      "event metadata"
    ),
    event_instruments = pull_redcap_table(
      redcap_event_instruments,
      redcap_uri,
      token,
      "event-instrument metadata"
    ),
    instruments = pull_redcap_table(
      redcap_instruments,
      redcap_uri,
      token,
      "instrument metadata"
    ),
    repeating_instruments = pull_redcap_table(
      redcap_instrument_repeating,
      redcap_uri,
      token,
      "repeating-instrument metadata"
    ),
    arms = pull_redcap_table(
      redcap_arm_export,
      redcap_uri,
      token,
      "arm metadata"
    ),
    project_info = pull_redcap_table(
      redcap_project_info_read,
      redcap_uri,
      token,
      "project metadata"
    )
  )
}
