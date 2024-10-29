#' @title Create a Record Status Dashboard Dataframe
#'
#' @description
#' [record_status_dashboard()] creates a dataframe that replicates the display of
#' the Record Status Dashboard from a REDCap project, given a REDCap API URI and token.
#' This function provides a structured overview of the status of records and their
#' associated instruments and events, similar to what is seen in the REDCap web interface.
#'
#' This output can be utilized for visualizations, such as heatmap displays, to gain
#' insights into data completeness and record status trends.
#'
#' @details
#' The REDCap Record Status Dashboard is a widely used tool for navigating and
#' understanding the state of records within a REDCap project, displaying their
#' completion statuses across various instruments and events. It can show whether
#' forms are marked as complete, in progress/unverified, incomplete, or not yet opened.
#'
#' [record_status_dashboard()] captures this information and provides it in a
#' structured dataframe format. By default, the function converts form completion
#' status into a percentage format, accounting for repeating instruments with overlapping
#' form status fields. This allows for a more flexible representation of the data,
#' especially useful when dealing with projects that utilize repeating forms or
#' multiple events.
#'
#' @return A dataframe representing the Record Status Dashboard view of a REDCap project.
#' Each row corresponds to a record, and columns represent different forms, events, and their
#' respective status indicators.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' redcap_uri <- Sys.getenv("REDCAP_URI")
#' token <- Sys.getenv("REDCAP_TOKEN")
#' record_status_dashboard(
#'   redcap_uri,
#'   token
#' )
#' }
record_status_dashboard <- function(supertbl) {
  has_arms <- "redcap_events" %in% names(supertbl)

  tidy_tbls <- prepare_tidy_tbls(supertbl)
  record_id_field <- REDCapTidieR:::get_record_id_field(supertbl$redcap_data[[1]])

  instrument_order <- factor(supertbl$redcap_form_label,
                             levels = supertbl$redcap_form_label, ordered = TRUE)

  # For Classic Databases ----
  if (!has_arms) {
    combined_data <- combine_data(tidy_tbls, record_id_field, instrument_order, has_arms)

    out <- reshape_data(combined_data, record_id_field = record_id_field)
  }

  # For Longitudinal Databases ----
  if (has_arms) {
    linked_arms <- do.call(rbind, supertbl$redcap_events) # TODO: Rename

    # Get the events to get event factor order
    # TODO: How to re-implement event factor order?
    events <-
      linked_arms |>
      dplyr::distinct()

    # Apply the function across each named element (sub-table) of `out`
    out <- map(tidy_tbls, join_linked_arms, linked_arms = linked_arms)

    combined_data <- combine_data(tidy_tbls, record_id_field, instrument_order, has_arms, linked_arms)

    out <- combined_data %>%
      reshape_data(record_id_field = record_id_field)
  }

  out
}

#' @title Prepare supertibble and tidy tibbles for record status dashboard processing
#'
#' @description Helper function that returns the supertibble and tidy tibbles
#' from it.
#'
#' @inheritParams REDCapTidieR::read_redcap
#'
#' @keywords internal
#'
#' @returns a list
prepare_tidy_tbls <- function(supertbl) {
  tidy_tbls <- supertbl %>%
    mutate(
      redcap_data = map2(
        .data$redcap_data,
        .data$redcap_form_name,
        ~ .x %>%
          mutate(
            redcap_form_name = .y,
            redcap_form_label = supertbl %>%
              filter(redcap_form_name == .y) %>%
              pull(redcap_form_label)
          )
      )
    ) %>%
    extract_tibbles()

  tidy_tbls
}

#' @title Join the a linked arms dataset onto a supertibble data tibble
#'
#' @description
#' Using a data output from [link_arms_rsd()], join onto each data tibble to make
#' the original event name accessible. During [read_redcap()] operations,
#' event names have an "arm" suffix that gets dropped. This looks to reinstate it
#' for proper links later on in the [record_status_dashboard()] logic.
#'
#' @param data_tbl a data tibble from a supertibble
#' @param linked_arms a dataframe output from [link_arms_rsd()] data
#'
#' @returns a data tibble appended with linked arms data
#'
#' @keywords internal
join_linked_arms <- function(data_tbl, linked_arms) {
  # Check if required columns exist in the sub-table
  if (all(c("redcap_form_name", "redcap_event") %in% colnames(data_tbl))) {

    # TODO: Necessary?
    data_tbl %>%
      # TODO: Fix this. Can cause artificial inflation of rows
      left_join(linked_arms, by = c("redcap_form_name" = "redcap_event"),
                relationship = "many-to-many") %>%
      select(infseq_id, form_status_complete, starts_with("redcap"))
  } else {
    # If columns don't exist, return the sub-table unchanged
    data_tbl
  }
}

#' @title Join supertibble datatibbles and arrange by instrument/event order
#'
#' @description This function takes a list of data tibbles and binds together
#' the columns necessary for the record status dashboard view:
#'
#' - the record ID
#' - the form completion status
#' - the unique event name
#' - the redcap form label
#' - the redcap event label (if longitudinal)
#'
#' @param data a named list of data tibbles from the supertbl
#' @param record_id_field the record ID field for the REDCap project identified by
#' [REDCapTidieR:::get_record_id_field()]
#' @param instrument_order a vector determining the factor level order of the project instruments
#' @param has_arms TRUE/FALSE whether or not the project is longitudinal/has arms
#' @param linked_arms if a longitudinal project, use the linked_arms data output from
#' [link_arms_rsd()]. Default `NULL`.
#'
#' @returns a dataframe
#'
#' @keywords internal
combine_data <- function(data,
                         record_id_field,
                         instrument_order,
                         has_arms,
                         linked_arms = NULL) {
  common_columns <- Reduce(intersect, lapply(data, names)) # nolint: object_usage_linter

  out <- data %>%
    map(~ select(.x, all_of(common_columns))) %>%
    bind_rows() %>%
    mutate(
      redcap_form_label = factor(.data$redcap_form_label,
                                 levels = levels(instrument_order),
                                 ordered = TRUE)
    )

  if (!has_arms) {
    out %>%
      dplyr::arrange(record_id_field, .data$redcap_form_label)
  } else {
    out %>%
      mutate(
        redcap_form_label = factor(.data$redcap_form_label,
                                   levels = levels(instrument_order),
                                   ordered = TRUE)
      ) %>%
      mutate(
        redcap_event_label = purrr::map_chr(.data$redcap_event,
                                            ~ get_event_name(.x, linked_arms = linked_arms))
        # TODO: Reimplement factor level order
      ) |>
      dplyr::arrange(record_id_field, .data$redcap_event_label)
  }
}

#' @noRd
get_event_name <- function(redcap_event, linked_arms) {
  linked_arms |>
    filter(.data$redcap_event == !!redcap_event) |>
    pull(.data$event_name) |>
    unique() |>
    as.character()
}

#' @title Rehsape combined data for record status dashboard display
#'
#' @description This function takes the output of [combine_data()] and performs
#' several pivoting operations to get the data in a format that yields only 3 columns:
#'
#' - the record ID field for the REDCap project
#' - the form name field, combined with the event field if applicable to a longitudinal project
#' - the completion status of that field, default as a percentage
#'
#' @param data a bound data tibble output from [combine_data()]
#' @param record_id_field the record ID field for the REDCap project identified by
#' [REDCapTidieR:::get_record_id_field()]
#'
#' @returns a dataframe
#'
#' @keywords internal
reshape_data <- function(data, record_id_field) {
  pivoted_data <- data %>%
    # Convert to percentage to temporarily handle repeat events
    mutate(
      .by = c(all_of(record_id_field), any_of(c("redcap_event_label", "redcap_form_label"))),
      form_percent_complete = mean(.data$form_status_complete == "Complete")
    ) |>
    select(-.data$form_status_complete) |>
    pivot_wider(
      id_cols = record_id_field,
      names_from = any_of(c("redcap_event_label", "redcap_form_label")),
      names_sep = " : ",
      values_from = .data$form_percent_complete,
      values_fn = unique
    )

  out <- pivoted_data %>%
    tidyr::pivot_longer(
      cols = -record_id_field, # All columns except infseq_id
      names_to = "form_name", # New column to store the names of the variables
      values_to = "pct_complete" # New column to store the values
    )

  out %>%
    mutate(
      !!sym(record_id_field) := factor(!!sym(record_id_field), levels = rev(unique(pivoted_data[[record_id_field]]))),
      form_name = factor(.data$form_name, levels = colnames(pivoted_data)[-1])
    )
}
