build_mock_status_dashboard <- function(
  data,
  metadata,
  events = tibble::tibble(),
  event_instruments = tibble::tibble(),
  instruments = tibble::tibble(),
  repeating_instruments = tibble::tibble()
) {
  project <- list(
    data = data,
    metadata = metadata,
    events = events,
    event_instruments = event_instruments,
    instruments = instruments,
    repeating_instruments = repeating_instruments
  )

  testthat::local_mocked_bindings(
    pull_redcap_project = function(redcap_uri, token) {
      expect_equal(redcap_uri, "https://redcap.example/api/")
      expect_equal(token, "test-token")
      project
    }
  )

  build_record_status_data(
    redcap_uri = "https://redcap.example/api/",
    token = "test-token"
  )
}

test_that("build_record_status_data requires REDCap API credentials", {
  expect_error(
    build_record_status_data(),
    "redcap_uri"
  )
  expect_error(
    build_record_status_data(redcap_uri = "", token = ""),
    "redcap_uri"
  )
})

test_that("build_record_status_data returns classic dashboard tiles", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    demographic_value = c("present", "present"),
    lab_value = c(NA, "pending"),
    demographics_complete = c(2, 2),
    labs_complete = c(NA, 1)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "demographic_value", "lab_value"),
    form_name = c("demographics", "demographics", "labs"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Demographic Value", "Lab Value")
  )

  instruments <- tibble::tibble(
    instrument_name = c("demographics", "labs"),
    instrument_label = c("Demographics", "Labs")
  )

  dashboard <- build_mock_status_dashboard(
    data = redcap_data,
    metadata = metadata,
    instruments = instruments
  )

  expect_named(dashboard, c("record_id", "form_name", "pct_complete"))
  expect_equal(nrow(dashboard), 4)
  expect_equal(levels(dashboard$record_id), c("2", "1"))
  expect_equal(levels(dashboard$form_name), c("Demographics", "Labs"))
  expect_equal(dashboard$pct_complete, c(1, NA, 1, 0))
})

test_that("build_record_status_data returns longitudinal event-form tiles", {
  redcap_data <- tibble::tibble(
    infseq_id = c("A", "A", "B", "B"),
    redcap_event_name = c(
      "baseline_arm_1",
      "followup_arm_1",
      "baseline_arm_1",
      "followup_arm_1"
    ),
    main_value = c("a", "b", "c", "d"),
    lab_value = c("low", NA, NA, NA),
    main_complete = c(2, 2, 2, NA),
    labs_complete = c(0, NA, NA, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("infseq_id", "main_value", "lab_value"),
    form_name = c("main", "main", "labs"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Lab Value")
  )

  events <- tibble::tibble(
    event_name = c("Baseline", "Follow-up"),
    unique_event_name = c("baseline_arm_1", "followup_arm_1"),
    arm_num = c(1L, 1L)
  )

  event_instruments <- tibble::tibble(
    unique_event_name = c("baseline_arm_1", "baseline_arm_1", "followup_arm_1"),
    form = c("main", "labs", "main"),
    arm_num = c(1L, 1L, 1L)
  )

  dashboard <- build_mock_status_dashboard(
    data = redcap_data,
    metadata = metadata,
    events = events,
    event_instruments = event_instruments
  )

  expect_named(
    dashboard,
    c("infseq_id", "event_name", "form_name", "pct_complete")
  )
  expect_equal(nrow(dashboard), 6)
  expect_equal(
    levels(dashboard$form_name),
    c("Baseline : main", "Baseline : labs", "Follow-up : main")
  )
  expect_false(any(
    dashboard$event_name == "followup_arm_1" & dashboard$form_name == "labs"
  ))
  expect_equal(
    dashboard |>
      dplyr::filter(
        .data$infseq_id == "B",
        .data$form_name == "Follow-up : main"
      ) |>
      dplyr::pull(.data$pct_complete),
    NA_real_
  )
})

test_that("build_record_status_data averages repeating instrument instances", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1", "2"),
    redcap_repeat_instrument = c(NA, "repeat_form", "repeat_form", NA),
    redcap_repeat_instance = c(NA, 1, 2, NA),
    main_value = c("present", NA, NA, "present"),
    repeat_value = c(NA, "first", "second", NA),
    main_complete = c(2, NA, NA, 2),
    repeat_form_complete = c(NA, 2, 0, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "repeat_value"),
    form_name = c("main", "main", "repeat_form"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Repeat Value")
  )

  dashboard <- build_mock_status_dashboard(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(form_name = "repeat_form")
  )

  expect_equal(
    dashboard |>
      dplyr::filter(.data$record_id == "1", .data$form_name == "repeat_form") |>
      dplyr::pull(.data$pct_complete),
    0.5
  )
  expect_equal(
    dashboard |>
      dplyr::filter(.data$record_id == "2", .data$form_name == "repeat_form") |>
      dplyr::pull(.data$pct_complete),
    NA_real_
  )
})

test_that("build_record_status_data includes repeating-instrument-only events", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_event_name = c(
      "repeating_event_arm_1",
      "nonrepeating_event_arm_1",
      "nonrepeating_event_arm_1"
    ),
    redcap_repeat_instrument = c(NA, "repeat_form", "repeat_form"),
    redcap_repeat_instance = c(1, 1, 2),
    event_value = c("event row", NA, NA),
    repeat_value = c(NA, "first", "second"),
    event_form_complete = c(2, NA, NA),
    repeat_form_complete = c(NA, 2, 0)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "event_value", "repeat_value"),
    form_name = c("event_form", "event_form", "repeat_form"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Event Value", "Repeat Value")
  )

  events <- tibble::tibble(
    event_name = c("Repeating Event", "Nonrepeating Event"),
    unique_event_name = c(
      "repeating_event_arm_1",
      "nonrepeating_event_arm_1"
    ),
    arm_num = c(1L, 1L)
  )

  event_instruments <- tibble::tibble(
    unique_event_name = c(
      "repeating_event_arm_1",
      "nonrepeating_event_arm_1"
    ),
    form = c("event_form", "repeat_form"),
    arm_num = c(1L, 1L)
  )

  repeating_instruments <- tibble::tibble(
    unique_event_name = c(
      "repeating_event_arm_1",
      "nonrepeating_event_arm_1"
    ),
    form_name = c(NA_character_, "repeat_form")
  )

  dashboard <- build_mock_status_dashboard(
    data = redcap_data,
    metadata = metadata,
    events = events,
    event_instruments = event_instruments,
    repeating_instruments = repeating_instruments
  )

  expect_equal(
    levels(dashboard$form_name),
    c("Repeating Event : event_form", "Nonrepeating Event : repeat_form")
  )
  expect_equal(
    dashboard |>
      dplyr::filter(.data$form_name == "Nonrepeating Event : repeat_form") |>
      dplyr::pull(.data$pct_complete),
    0.5
  )
})

test_that("plot_record_status returns a ggplot heat map", {
  dashboard <- tibble::tibble(
    record_id = factor(c("2", "2", "1", "1"), levels = c("2", "1")),
    form_name = factor(
      c("Demographics", "Labs", "Demographics", "Labs"),
      levels = c("Demographics", "Labs")
    ),
    pct_complete = c(1, 0.5, 1, NA)
  )

  plot <- plot_record_status(dashboard)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "REDCap Form")
  expect_equal(plot$labels$y, "Record ID")
  expect_equal(plot$labels$fill, "Completion Status")
})

test_that("plot_record_status supports explicit record ID columns", {
  dashboard <- tibble::tibble(
    infseq_id = c("A", "B"),
    form_name = c("Baseline : Main", "Baseline : Main"),
    pct_complete = c(1, 0)
  )

  plot <- plot_record_status(
    dashboard,
    record_id_field = "infseq_id",
    x_label = "Form",
    y_label = "INFSEQ ID",
    fill_label = "Complete"
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$x, "Form")
  expect_equal(plot$labels$y, "INFSEQ ID")
  expect_equal(plot$labels$fill, "Complete")
})

test_that("plot_record_status supports compact mode", {
  dashboard <- tibble::tibble(
    record_id = factor(c("2", "2", "1", "1"), levels = c("2", "1")),
    form_name = factor(
      c("Demographics", "Labs", "Demographics", "Labs"),
      levels = c("Demographics", "Labs")
    ),
    pct_complete = c(1, 0.5, 1, NA)
  )

  plot <- plot_record_status(dashboard, mode = "compact")
  built_plot <- ggplot2::ggplot_build(plot)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$y, "Record ID")
  expect_equal(built_plot$layout$panel_params[[1]]$y$get_labels(), c("2", "1"))
})

test_that("plot_record_status does not thin ticks by default", {
  dashboard <- tibble::tibble(
    record_id = as.character(seq_len(5)),
    form_name = paste0("form_", seq_len(5)),
    pct_complete = 1
  )

  standard_plot <- plot_record_status(dashboard)
  compact_plot <- plot_record_status(dashboard, mode = "compact")
  standard_built <- ggplot2::ggplot_build(standard_plot)
  compact_built <- ggplot2::ggplot_build(compact_plot)

  expected_x_labels <- paste0("form_", seq_len(5))
  expect_equal(
    standard_built$layout$panel_params[[1]]$x$get_labels(),
    expected_x_labels
  )
  expect_equal(
    compact_built$layout$panel_params[[1]]$x$get_labels(),
    expected_x_labels
  )
  expect_null(standard_plot$scales$get_scales("y"))
  expect_null(compact_plot$scales$get_scales("y"))
})

test_that("plot_record_status thins x ticks in standard mode", {
  dashboard <- tibble::tibble(
    record_id = "1",
    form_name = paste0("form_", seq_len(25)),
    pct_complete = 1
  )

  plot <- plot_record_status(dashboard, x_tick_every = 10)
  built_plot <- ggplot2::ggplot_build(plot)

  expect_equal(
    built_plot$layout$panel_params[[1]]$x$get_labels(),
    c("form_1", "form_11", "form_21")
  )
})

test_that("plot_record_status thins y ticks in standard mode", {
  dashboard <- tibble::tibble(
    record_id = as.character(seq_len(30)),
    form_name = "Demographics",
    pct_complete = 1
  )

  plot <- plot_record_status(dashboard, y_tick_every = 5)
  built_plot <- ggplot2::ggplot_build(plot)

  expect_equal(
    built_plot$layout$panel_params[[1]]$y$get_labels(),
    c("30", "25", "20", "15", "10", "5")
  )
})

test_that("plot_record_status thins compact ticks without dropping tiles", {
  dashboard <- tidyr::expand_grid(
    record_id = as.character(seq_len(30)),
    form_name = paste0("form_", seq_len(25))
  ) |>
    dplyr::mutate(pct_complete = 1)

  plot <- plot_record_status(
    dashboard,
    mode = "compact",
    x_tick_every = 10,
    y_tick_every = 5
  )
  built_plot <- ggplot2::ggplot_build(plot)

  expect_equal(
    built_plot$layout$panel_params[[1]]$x$get_labels(),
    c("form_1", "form_11", "form_21")
  )
  expect_equal(
    built_plot$layout$panel_params[[1]]$y$get_labels(),
    c("30", "25", "20", "15", "10", "5")
  )
  expect_equal(nrow(built_plot$data[[1]]), nrow(dashboard))
})

test_that("plot_record_status truncates long compact form labels", {
  long_label <- "Baseline Event : Very Long REDCap Instrument Label"
  dashboard <- tibble::tibble(
    record_id = "1",
    form_name = long_label,
    pct_complete = 1
  )

  standard_plot <- plot_record_status(dashboard)
  compact_plot <- plot_record_status(dashboard, mode = "compact")
  override_plot <- plot_record_status(
    dashboard,
    mode = "compact",
    form_label_max = 20
  )

  expect_equal(levels(standard_plot$data$form_name), long_label)
  expect_equal(
    ggplot2::ggplot_build(compact_plot)$layout$panel_params[[1]]$x$get_labels(),
    stringr::str_trunc(long_label, width = 35)
  )
  expect_equal(
    ggplot2::ggplot_build(override_plot)$layout$panel_params[[1]]$x$get_labels(),
    stringr::str_trunc(long_label, width = 20)
  )
  expect_equal(levels(compact_plot$data$form_name), long_label)
})

test_that("plot_record_status keeps truncated form labels distinct", {
  labels <- c(
    "A very long form label whose shared prefix is identical one",
    "A very long form label whose shared prefix is identical two"
  )
  dashboard <- tibble::tibble(
    record_id = "1",
    form_name = labels,
    pct_complete = c(1, 0)
  )

  plot <- plot_record_status(dashboard, mode = "compact", form_label_max = 35)
  built_plot <- ggplot2::ggplot_build(plot)

  expect_equal(nlevels(plot$data$form_name), 2)
  expect_equal(built_plot$data[[1]]$x, c(1, 2))
})

test_that("plot_record_status compact mode preserves explicit overrides", {
  dashboard <- tibble::tibble(
    record_id = as.character(seq_len(100)),
    form_name = "Demographics",
    pct_complete = 1
  )

  plot <- plot_record_status(
    dashboard,
    mode = "compact",
    tile_linewidth = 0.4,
    x_text_size = 9,
    y_text_size = 8,
    show_y_title = TRUE,
    form_label_max = NULL
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(levels(plot$data$form_name), "Demographics")
})

test_that("plot_record_status allows compact y-axis overrides", {
  dashboard <- tibble::tibble(
    record_id = as.character(seq_len(3)),
    form_name = "Demographics",
    pct_complete = 1
  )

  plot <- plot_record_status(
    dashboard,
    mode = "compact",
    show_y_title = FALSE
  )

  expect_s3_class(plot$theme$axis.title.y, "element_blank")
})

test_that("plot_record_status validates dashboard data", {
  expect_error(
    plot_record_status(tibble::tibble(form_name = "main")),
    "pct_complete"
  )
  expect_error(
    plot_record_status(
      tibble::tibble(
        record_id = "1",
        alternate_id = "A",
        form_name = "main",
        pct_complete = 1
      )
    ),
    "record ID"
  )
  expect_error(
    plot_record_status(
      tibble::tibble(
        record_id = "1",
        form_name = "main",
        pct_complete = 1
      ),
      record_id_field = "missing_id"
    ),
    "record_id_field"
  )
  expect_error(
    plot_record_status(
      tibble::tibble(record_id = "1", form_name = "main", pct_complete = 1),
      x_tick_every = 0
    ),
    "x_tick_every"
  )
  expect_error(
    plot_record_status(
      tibble::tibble(record_id = "1", form_name = "main", pct_complete = 1),
      y_tick_every = 1.5
    ),
    "y_tick_every"
  )
  expect_error(
    plot_record_status(
      tibble::tibble(record_id = "1", form_name = "main", pct_complete = 1.1)
    ),
    "between 0 and 1"
  )
})
