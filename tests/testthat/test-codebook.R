build_mock_codebook <- function(
  metadata,
  events = tibble::tibble(),
  event_instruments = tibble::tibble(),
  instruments = tibble::tibble(),
  repeating_instruments = tibble::tibble(),
  arms = tibble::tibble(),
  project_info = tibble::tibble()
) {
  project <- list(
    metadata = metadata,
    events = events,
    event_instruments = event_instruments,
    instruments = instruments,
    repeating_instruments = repeating_instruments,
    arms = arms,
    project_info = project_info
  )

  testthat::local_mocked_bindings(
    pull_redcap_codebook = function(redcap_uri, token) {
      expect_equal(redcap_uri, "https://redcap.example/api/")
      expect_equal(token, "test-token")
      project
    }
  )

  build_codebook(
    redcap_uri = "https://redcap.example/api/",
    token = "test-token"
  )
}

test_that("build_codebook requires REDCap API credentials", {
  expect_error(
    build_codebook(),
    "redcap_uri"
  )
  expect_error(
    build_codebook(redcap_uri = "", token = ""),
    "redcap_uri"
  )
})

test_that("pull_redcap_codebook quiets REDCapR API messages", {
  quiet_required <- function(redcap_uri, token, verbose, ...) {
    if (verbose) {
      message("required metadata should be quiet")
    }
    list(
      data = tibble::tibble(
        field_name = "record_id",
        form_name = "main",
        field_type = "text",
        field_label = "Record ID"
      )
    )
  }
  quiet_optional <- function(redcap_uri, token, verbose, ...) {
    if (verbose) {
      message("optional metadata should be quiet")
    }
    list(data = tibble::tibble())
  }

  testthat::local_mocked_bindings(
    redcap_metadata_read = quiet_required,
    redcap_event_read = quiet_optional,
    redcap_event_instruments = quiet_optional,
    redcap_instruments = quiet_optional,
    redcap_instrument_repeating = quiet_optional,
    redcap_arm_export = quiet_optional,
    redcap_project_info_read = quiet_optional
  )

  expect_message(
    project <- pull_redcap_codebook(
      redcap_uri = "https://redcap.example/api/",
      token = "test-token"
    ),
    NA
  )
  expect_named(
    project,
    c(
      "metadata",
      "events",
      "event_instruments",
      "instruments",
      "repeating_instruments",
      "arms",
      "project_info"
    )
  )
})

test_that("build_codebook returns structured codebook tables", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "sex", "visit_date", "symptoms"),
    form_name = c("demographics", "demographics", "visit", "visit"),
    field_type = c("text", "radio", "text", "checkbox"),
    field_label = c("Record ID", "Sex", "Visit Date", "Symptoms"),
    required_field = c("y", "n", "y", "n"),
    identifier = c("n", "n", "n", "n"),
    select_choices_or_calculations = c(
      NA,
      "1, Female | 2, Male | 3, Unknown, not reported",
      NA,
      "fever, Fever | rash, Rash"
    ),
    text_validation_type_or_show_slider_number = c(NA, NA, "date_ymd", NA),
    text_validation_min = c(NA, NA, "2020-01-01", NA),
    text_validation_max = c(NA, NA, "2030-12-31", NA),
    branching_logic = c(NA, NA, NA, "[sex] = '1'"),
    field_note = c(NA, NA, "Use visit date.", NA)
  )
  events <- tibble::tibble(
    event_name = c("Baseline", "Follow-up"),
    arm_num = c(1, 1),
    unique_event_name = c("baseline_arm_1", "followup_arm_1")
  )
  event_instruments <- tibble::tibble(
    unique_event_name = c("baseline_arm_1", "baseline_arm_1", "followup_arm_1"),
    instrument = c("demographics", "visit", "visit")
  )
  instruments <- tibble::tibble(
    instrument_name = c("demographics", "visit"),
    instrument_label = c("Demographics", "Visit")
  )
  repeating <- tibble::tibble(
    event_name = "Follow-up",
    instrument_name = "visit",
    unique_event_name = "followup_arm_1"
  )
  project_info <- tibble::tibble(
    project_id = 42,
    project_title = "Example Codebook",
    in_production = FALSE,
    project_language = "English",
    surveys_enabled = TRUE
  )

  codebook <- build_mock_codebook(
    metadata = metadata,
    events = events,
    event_instruments = event_instruments,
    instruments = instruments,
    repeating_instruments = repeating,
    project_info = project_info
  )

  expect_s3_class(codebook, "redcap_codebook")
  expect_named(
    codebook,
    c(
      "fields",
      "choices",
      "forms",
      "events",
      "event_instruments",
      "repeating",
      "project"
    )
  )
  expect_equal(nrow(codebook$fields), 4)
  expect_equal(nrow(codebook$choices), 5)
  expect_equal(codebook$project$project_title, "Example Codebook")
  expect_equal(codebook$project$project_id, 42)
  expect_equal(codebook$project$project_language, "English")
  expect_true(codebook$project$surveys_enabled)
  expect_equal(codebook$project$field_count, 4)
  expect_equal(codebook$project$form_count, 2)
  expect_equal(codebook$project$event_count, 2)
  expect_true(codebook$project$repeating_enabled)
})

test_that("build_codebook parses choices and field display metadata", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "sex", "visit_date", "symptoms"),
    form_name = c("demographics", "demographics", "visit", "visit"),
    field_type = c("text", "radio", "text", "checkbox"),
    field_label = c("Record ID", "Sex", "Visit Date", "Symptoms"),
    required_field = c("y", "n", "y", "n"),
    identifier = c("y", "n", "n", "n"),
    select_choices_or_calculations = c(
      NA,
      "1, Female | 2, Male | 3, Unknown, not reported",
      NA,
      "fever, Fever | rash, Rash"
    ),
    text_validation_type_or_show_slider_number = c(NA, NA, "date_ymd", NA),
    text_validation_min = c(NA, NA, "2020-01-01", NA),
    text_validation_max = c(NA, NA, "2030-12-31", NA),
    branching_logic = c(NA, NA, NA, "[sex] = '1'")
  )

  codebook <- build_mock_codebook(metadata)

  expect_equal(
    codebook$choices |>
      dplyr::filter(.data$field_name == "sex") |>
      dplyr::pull(.data$choice_label),
    c("Female", "Male", "Unknown, not reported")
  )
  expect_equal(
    codebook$fields |>
      dplyr::filter(.data$field_name == "visit_date") |>
      dplyr::pull(.data$validation),
    "date_ymd; min 2020-01-01; max 2030-12-31"
  )
  expect_equal(
    codebook$fields |>
      dplyr::filter(.data$field_name == "record_id") |>
      dplyr::pull(.data$identifier),
    TRUE
  )
  expect_match(
    codebook$fields |>
      dplyr::filter(.data$field_name == "symptoms") |>
      dplyr::pull(.data$choices),
    "fever = Fever"
  )
})

test_that("build_codebook summarizes forms, events, and repeating structure", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "baseline_value", "visit_value"),
    form_name = c("baseline", "baseline", "visit"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Baseline Value", "Visit Value")
  )
  events <- tibble::tibble(
    event_name = c("Baseline", "Month 1"),
    arm_num = c(1, 1),
    unique_event_name = c("baseline_arm_1", "month_1_arm_1")
  )
  event_instruments <- tibble::tibble(
    unique_event_name = c("baseline_arm_1", "month_1_arm_1"),
    instrument = c("baseline", "visit")
  )
  instruments <- tibble::tibble(
    instrument_name = c("baseline", "visit"),
    instrument_label = c("Baseline", "Visit")
  )
  repeating <- tibble::tibble(
    instrument_name = "visit",
    unique_event_name = "month_1_arm_1"
  )

  codebook <- build_mock_codebook(
    metadata = metadata,
    events = events,
    event_instruments = event_instruments,
    instruments = instruments,
    repeating_instruments = repeating
  )

  expect_equal(
    codebook$forms,
    tibble::tibble(
      form_order = c(1L, 2L),
      form_name = c("baseline", "visit"),
      form_label = c("Baseline", "Visit"),
      field_count = c(2L, 1L)
    )
  )
  expect_equal(
    codebook$fields |>
      dplyr::filter(.data$form_name == "visit") |>
      dplyr::pull(.data$event_names),
    "month_1_arm_1"
  )
  expect_equal(
    codebook$fields |>
      dplyr::filter(.data$form_name == "visit") |>
      dplyr::pull(.data$repeating_status),
    "Repeating instrument"
  )
})

test_that("print.redcap_codebook reports concise project counts", {
  metadata <- tibble::tibble(
    field_name = "record_id",
    form_name = "main",
    field_type = "text",
    field_label = "Record ID"
  )
  project_info <- tibble::tibble(project_title = "Print Test")
  codebook <- build_mock_codebook(metadata, project_info = project_info)

  expect_output(
    print(codebook),
    "Project: Print Test"
  )
})

test_that("view_codebook validates inputs", {
  expect_error(
    view_codebook(tibble::tibble()),
    "redcap_codebook"
  )

  metadata <- tibble::tibble(
    field_name = "record_id",
    form_name = "main",
    field_type = "text",
    field_label = "Record ID"
  )
  codebook <- build_mock_codebook(metadata)

  expect_error(
    view_codebook(codebook, page_length = 0),
    "page_length"
  )
  expect_error(
    view_codebook(codebook, page_length = 1.5),
    "page_length"
  )
})

test_that("view_codebook returns a browsable HTML object", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "status"),
    form_name = c("main", "main"),
    field_type = c("text", "dropdown"),
    field_label = c("Record ID", "Status"),
    select_choices_or_calculations = c(NA, "1, Open | 2, Closed")
  )
  project_info <- tibble::tibble(
    project_id = 42,
    project_title = "Viewer Test Project",
    in_production = FALSE,
    project_language = "English",
    surveys_enabled = TRUE
  )
  codebook <- build_mock_codebook(metadata, project_info = project_info)

  viewer <- view_codebook(codebook, page_length = 5)

  expect_s3_class(viewer, "shiny.tag.list")
  expect_true(isTRUE(attr(viewer, "browsable_html")))
  expect_match(as.character(viewer), "Viewer Test Project")
  expect_match(as.character(viewer), "REDCap Codebook")
  expect_match(as.character(viewer), "searched, sorted, paged")
  expect_false(grepl(
    "Link to Project Home",
    as.character(viewer),
    fixed = TRUE
  ))
  expect_match(as.character(viewer), "redcap-codebook-summary")
  expect_match(as.character(viewer), "redcap-codebook-project-details")
  expect_match(as.character(viewer), "Project ID")
  expect_match(as.character(viewer), "Project features")
  expect_match(as.character(viewer), "Surveys enabled")
  expect_match(as.character(viewer), "data-section=\"fields\"")
  expect_match(as.character(viewer), "data-section=\"choices\"")
  expect_match(as.character(viewer), "redcap-codebook-tab-count")
  expect_match(as.character(viewer), "2 rows")
  expect_match(as.character(viewer), "redcap-codebook-table-panel")
  expect_false(grepl(
    "data-section=\"events\"",
    as.character(viewer),
    fixed = TRUE
  ))
  expect_false(grepl("Event Instruments", as.character(viewer), fixed = TRUE))
  expect_false(
    grepl("data-section=\"repeating\"", as.character(viewer), fixed = TRUE)
  )
  expect_match(as.character(viewer), "role=\"tabpanel\"")
  expect_false(grepl("Back to top", as.character(viewer), fixed = TRUE))
  expect_match(as.character(viewer), "status")
  expect_match(as.character(viewer), "Open")
  viewer_head <- htmltools::renderTags(viewer)$head
  expect_match(viewer_head, "HTMLWidgets.staticRender")
  expect_match(viewer_head, "columns.adjust")
  expect_match(viewer_head, "scrollIntoView")
  expect_match(viewer_head, "position: sticky")
  expect_match(viewer_head, "redcap-codebook-summary-item")
  expect_match(viewer_head, "redcap-codebook-table thead th")
  expect_match(viewer_head, "redcap-codebook-project-detail-table")
  expect_match(viewer_head, "redcap-codebook-row-selected")
  expect_false(grepl("window.location.hash =", viewer_head, fixed = TRUE))
})

test_that("view_codebook includes section descriptions", {
  metadata <- tibble::tibble(
    field_name = "record_id",
    form_name = "main",
    field_type = "text",
    field_label = "Record ID"
  )
  codebook <- build_mock_codebook(metadata)

  viewer <- view_codebook(codebook)

  expect_match(as.character(viewer), "A one-row overview")
  expect_match(as.character(viewer), "One row per REDCap field")
  expect_match(as.character(viewer), "One row per REDCap instrument")
  expect_match(
    as.character(viewer),
    "No coded choice options were found in this project."
  )
})

test_that("view_codebook omits empty optional event and repeating sections", {
  metadata <- tibble::tibble(
    field_name = "record_id",
    form_name = "main",
    field_type = "text",
    field_label = "Record ID"
  )
  codebook <- build_mock_codebook(metadata)

  expect_no_error(viewer <- view_codebook(codebook))
  expect_false(grepl(
    "data-section=\"events\"",
    as.character(viewer),
    fixed = TRUE
  ))
  expect_false(
    grepl(
      "data-section=\"event_instruments\"",
      as.character(viewer),
      fixed = TRUE
    )
  )
  expect_false(
    grepl("data-section=\"repeating\"", as.character(viewer), fixed = TRUE)
  )
})

test_that("view_codebook includes populated optional sections", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "visit_date"),
    form_name = c("main", "visit"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Visit Date")
  )
  events <- tibble::tibble(
    event_name = "Baseline",
    arm_num = 1,
    unique_event_name = "baseline_arm_1"
  )
  event_instruments <- tibble::tibble(
    unique_event_name = "baseline_arm_1",
    instrument = "visit"
  )
  repeating <- tibble::tibble(
    unique_event_name = "baseline_arm_1",
    instrument_name = "visit"
  )
  codebook <- build_mock_codebook(
    metadata = metadata,
    events = events,
    event_instruments = event_instruments,
    repeating_instruments = repeating
  )

  viewer <- view_codebook(codebook)

  expect_match(as.character(viewer), "data-section=\"events\"")
  expect_match(as.character(viewer), "data-section=\"event_instruments\"")
  expect_match(as.character(viewer), "data-section=\"repeating\"")
})

test_that("view_codebook escapes REDCap metadata values as text", {
  metadata <- tibble::tibble(
    field_name = "record_id",
    form_name = "main",
    field_type = "text",
    field_label = "<script>alert('x')</script>"
  )
  codebook <- build_mock_codebook(metadata)

  viewer <- view_codebook(codebook)

  expect_match(as.character(viewer), "&lt;script&gt;alert")
  expect_false(grepl("<script>alert", as.character(viewer), fixed = TRUE))
})
