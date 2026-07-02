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

  record_status_dashboard(
    redcap_uri = "https://redcap.example/api/",
    token = "test-token"
  )
}

test_that("record_status_dashboard requires REDCap API credentials", {
  expect_error(
    record_status_dashboard(),
    "redcap_uri"
  )
  expect_error(
    record_status_dashboard(redcap_uri = "", token = ""),
    "redcap_uri"
  )
})

test_that("record_status_dashboard returns classic dashboard tiles", {
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

test_that("record_status_dashboard returns longitudinal event-form tiles", {
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

test_that("record_status_dashboard averages repeating instrument instances", {
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
    metadata = metadata
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
