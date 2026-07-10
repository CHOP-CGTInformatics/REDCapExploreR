#' @title Plot Record Status Dashboard data
#'
#' @description
#' `plot_record_status()` creates a ggplot heat map from data returned by
#' [get_record_status_data()]. The plot is designed to resemble the REDCap
#' Record Status Dashboard while remaining a standard ggplot object that users
#' can extend with additional ggplot2 layers.
#'
#' @details
#' The function expects one row per dashboard tile with a `form_name` column and
#' a `pct_complete` column. The record ID column is inferred from the remaining
#' data columns when possible. For longitudinal output, `event_name` is ignored
#' because event labels are already included in `form_name`.
#'
#' `pct_complete` should range from `0` to `1`, where `1` is complete and `0` is
#' incomplete or unverified. Missing values are shown with `missing_color`.
#'
#' @param data A dataframe returned by [get_record_status_data()].
#' @param record_id_field Optional string naming the record ID column. By
#'   default, the function infers the only column other than `form_name`,
#'   `event_name`, and `pct_complete`.
#' @param mode Plot display mode. Use `"standard"` for the default dashboard
#'   view or `"compact"` for smaller x-axis text, thinner tile borders, and
#'   truncated long form labels. Compact mode preserves the default y-axis
#'   labels and title unless those are overridden directly.
#' @param low_color Color for `pct_complete = 0`.
#' @param mid_color Color for `pct_complete = 0.5`.
#' @param high_color Color for `pct_complete = 1`.
#' @param missing_color Fill color for missing `pct_complete` values.
#' @param tile_color Border color for each dashboard tile.
#' @param tile_linewidth Border width for each dashboard tile. When `NULL`,
#'   the value is chosen from `mode`.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param fill_label Fill legend label.
#' @param rotate_x_text Whether to rotate form labels on the top axis.
#' @param x_text_size,y_text_size Optional axis text sizes.
#' @param x_tick_every,y_tick_every Optional positive integers for showing every
#'   nth x- or y-axis tick label. When `NULL`, the axis is not thinned. These
#'   arguments work in both standard and compact modes.
#' @param show_y_title Whether to show the y-axis title.
#' @param form_label_max Optional positive integer for truncating long form
#'   labels. When `NULL`, compact mode truncates labels to 35 characters and
#'   standard mode leaves labels unchanged.
#'
#' @returns A ggplot object.
#'
#' @examples
#' plot_record_status(mock_record_status_data)
#'
#' plot_record_status(mock_record_status_data, mode = "compact")
#'
#' \dontrun{
#' data <- get_record_status_data(
#'   redcap_uri = Sys.getenv("REDCAP_URI"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#'
#' plot_record_status(data)
#' }
#'
#' @export
plot_record_status <- function(
  data,
  record_id_field = NULL,
  mode = c("standard", "compact"),
  low_color = "#D73027",
  mid_color = "#FEE08B",
  high_color = "#1A9850",
  missing_color = "grey95",
  tile_color = "white",
  tile_linewidth = NULL,
  x_label = "REDCap Form",
  y_label = "Record ID",
  fill_label = "Completion Status",
  rotate_x_text = TRUE,
  x_text_size = NULL,
  y_text_size = NULL,
  x_tick_every = NULL,
  y_tick_every = NULL,
  show_y_title = NULL,
  form_label_max = NULL
) {
  mode <- match.arg(mode)
  plot_data <- get_record_status_plot_data(
    data = data,
    record_id_field = record_id_field
  )
  record_id_field <- attr(plot_data, "record_id_field")
  settings <- get_status_plot_settings(
    mode = mode,
    tile_linewidth = tile_linewidth,
    x_text_size = x_text_size,
    y_text_size = y_text_size,
    x_tick_every = x_tick_every,
    y_tick_every = y_tick_every,
    show_y_title = show_y_title,
    form_label_max = form_label_max
  )
  plot_data <- get_compact_form_labels(plot_data, settings$form_label_max)

  x_text <- if (rotate_x_text) {
    element_text(
      angle = 90,
      hjust = 0,
      vjust = 0.5,
      size = settings$x_text_size
    )
  } else {
    element_text(size = settings$x_text_size)
  }
  y_text <- if (is.null(settings$y_text_size)) {
    element_text()
  } else {
    element_text(size = settings$y_text_size)
  }
  y_title <- if (settings$show_y_title) element_text() else element_blank()

  x_scale <- if (is.null(settings$x_breaks)) {
    scale_x_discrete(position = "top", drop = FALSE)
  } else {
    scale_x_discrete(
      position = "top",
      breaks = settings$x_breaks,
      drop = FALSE
    )
  }

  out <- ggplot(
    plot_data,
    aes(
      x = .data$form_name,
      y = .data[[record_id_field]],
      fill = .data$pct_complete
    )
  ) +
    geom_tile(color = tile_color, linewidth = settings$tile_linewidth) +
    scale_fill_gradientn(
      colors = c(low_color, mid_color, high_color),
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0%", "50%", "100%"),
      na.value = missing_color
    ) +
    x_scale

  if (!is.null(settings$y_breaks)) {
    out <- out +
      scale_y_discrete(breaks = settings$y_breaks, drop = FALSE)
  }

  out +
    theme_minimal() +
    theme(
      axis.text.x = x_text,
      axis.text.y = y_text,
      axis.title.y = y_title,
      panel.grid = element_blank()
    ) +
    labs(
      x = x_label,
      y = y_label,
      fill = fill_label
    )
}

#' Resolve plot defaults for standard and compact display modes
#'
#' @param mode Plot mode.
#' @param tile_linewidth,x_text_size,y_text_size,x_tick_every,y_tick_every,
#'   show_y_title,form_label_max User supplied plotting overrides.
#'
#' @returns A list of concrete plotting settings.
#'
#' @noRd
get_status_plot_settings <- function(
  mode,
  tile_linewidth = NULL,
  x_text_size = NULL,
  y_text_size = NULL,
  x_tick_every = NULL,
  y_tick_every = NULL,
  show_y_title = NULL,
  form_label_max = NULL
) {
  if (is.null(tile_linewidth)) {
    tile_linewidth <- if (mode == "compact") 0.05 else 0.25
  }
  if (is.null(x_text_size) && mode == "compact") {
    x_text_size <- 6
  }
  if (is.null(show_y_title)) {
    show_y_title <- TRUE
  }
  if (is.null(form_label_max) && mode == "compact") {
    form_label_max <- 35L
  }

  x_tick_every <- get_record_status_integer(x_tick_every, "x_tick_every")
  y_tick_every <- get_record_status_integer(y_tick_every, "y_tick_every")
  form_label_max <- get_record_status_integer(form_label_max, "form_label_max")
  x_breaks <- get_record_status_breaks(x_tick_every)
  y_breaks <- get_record_status_breaks(y_tick_every)

  list(
    tile_linewidth = tile_linewidth,
    x_text_size = x_text_size,
    y_text_size = y_text_size,
    x_tick_every = x_tick_every,
    y_tick_every = y_tick_every,
    x_breaks = x_breaks,
    y_breaks = y_breaks,
    show_y_title = show_y_title,
    form_label_max = form_label_max
  )
}

#' Build a discrete scale breaks function
#'
#' @param tick_every Positive integer tick interval.
#'
#' @returns A breaks function or `NULL`.
#'
#' @noRd
get_record_status_breaks <- function(tick_every = NULL) {
  if (is.null(tick_every)) {
    return(NULL)
  }

  function(x) x[seq(1, length(x), by = tick_every)]
}

#' Truncate form label factor levels when requested
#'
#' @param data Plot data.
#' @param form_label_max Maximum form label width.
#'
#' @returns Plot data with updated `form_name` factor labels.
#'
#' @noRd
get_compact_form_labels <- function(data, form_label_max = NULL) {
  if (is.null(form_label_max)) {
    return(data)
  }

  labels <- levels(data$form_name)
  levels(data$form_name) <- str_trunc(labels, width = form_label_max)
  data
}

#' Validate a nullable positive integer argument
#'
#' @param value Argument value.
#' @param arg Argument name for error messages.
#'
#' @returns `NULL` or an integer.
#'
#' @noRd
get_record_status_integer <- function(value, arg) {
  if (is.null(value)) {
    return(NULL)
  }

  valid <- is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    value >= 1 &&
    value == floor(value)

  if (!valid) {
    cli_abort("{.arg {arg}} must be a positive integer.")
  }

  as.integer(value)
}

#' Validate and normalize record status data for plotting
#'
#' @param data A dataframe.
#' @param record_id_field Optional string naming the record ID column.
#'
#' @returns A tibble with plotting columns and a `record_id_field` attribute.
#'
#' @noRd
get_record_status_plot_data <- function(data, record_id_field = NULL) {
  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  data <- as_tibble(data)
  required_columns <- c("form_name", "pct_complete")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    cli_abort("{.arg data} must include {.field {missing_columns}}.")
  }

  record_id_field <- get_record_status_id_field(data, record_id_field)

  data <- data |>
    mutate(
      form_name = get_record_status_plot_factor(.data$form_name),
      !!record_id_field := get_record_status_plot_factor(
        .data[[record_id_field]],
        reverse = TRUE
      )
    )

  attr(data, "record_id_field") <- record_id_field
  data
}

#' Identify the record ID column in record status data
#'
#' @inheritParams get_record_status_plot_data
#'
#' @returns A string naming the record ID column.
#'
#' @noRd
get_record_status_id_field <- function(data, record_id_field = NULL) {
  if (!is.null(record_id_field)) {
    if (
      !is.character(record_id_field) ||
        length(record_id_field) != 1 ||
        is.na(record_id_field) ||
        record_id_field == ""
    ) {
      cli_abort("{.arg record_id_field} must be a single column name.")
    }

    if (!record_id_field %in% names(data)) {
      cli_abort("{.arg record_id_field} must name a column in {.arg data}.")
    }

    return(record_id_field)
  }

  candidate_fields <- setdiff(
    names(data),
    c("event_name", "form_name", "pct_complete")
  )
  if (length(candidate_fields) != 1) {
    cli_abort(
      paste(
        "Could not infer the record ID column.",
        "Use {.arg record_id_field} to name it explicitly."
      )
    )
  }

  candidate_fields[[1]]
}

#' Preserve existing factor order or create one from observed values
#'
#' @param value A vector to convert to a factor.
#' @param reverse Whether to reverse observed values when creating factor
#'   levels.
#'
#' @returns A factor.
#'
#' @noRd
get_record_status_plot_factor <- function(value, reverse = FALSE) {
  if (is.factor(value)) {
    return(value)
  }

  levels <- unique(value)
  if (reverse) {
    levels <- rev(levels)
  }

  factor(value, levels = levels)
}
