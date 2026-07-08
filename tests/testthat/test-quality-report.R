build_mock_quality_report <- function(data,
                                      metadata,
                                      events = tibble::tibble(),
                                      event_instruments = tibble::tibble(),
                                      instruments = tibble::tibble(),
                                      repeating_instruments = tibble::tibble(),
                                      checks = c(
                                        "missingness",
                                        "metadata",
                                        "outliers",
                                        "operational",
                                        "consistency"
                                      ),
                                      progress = "none",
                                      ...) {
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

  build_quality_report(
    redcap_uri = "https://redcap.example/api/",
    token = "test-token",
    checks = checks,
    progress = progress,
    ...
  )
}

test_that("build_quality_report requires REDCap API credentials", {
  expect_error(
    build_quality_report(progress = "none"),
    "redcap_uri"
  )
  expect_error(
    build_quality_report(redcap_uri = "", token = "", progress = "none"),
    "redcap_uri"
  )
})

test_that("pull_redcap_project quiets REDCapR API messages", {
  quiet_records <- function(redcap_uri, token, verbose, ...) {
    if (verbose) {
      message("records should be quiet")
    }
    list(data = tibble::tibble(record_id = "1"))
  }
  quiet_metadata <- function(redcap_uri, token, verbose, ...) {
    if (verbose) {
      message("metadata should be quiet")
    }
    list(data = tibble::tibble(
      field_name = "record_id",
      form_name = "main",
      field_type = "text",
      field_label = "Record ID"
    ))
  }
  quiet_optional <- function(redcap_uri, token, verbose, ...) {
    if (verbose) {
      message("optional metadata should be quiet")
    }
    list(data = tibble::tibble())
  }

  testthat::local_mocked_bindings(
    redcap_read_oneshot = quiet_records,
    redcap_metadata_read = quiet_metadata,
    redcap_event_read = quiet_optional,
    redcap_event_instruments = quiet_optional,
    redcap_instruments = quiet_optional,
    redcap_instrument_repeating = quiet_optional
  )

  expect_message(
    project <- pull_redcap_project(
      redcap_uri = "https://redcap.example/api/",
      token = "test-token"
    ),
    NA
  )
  expect_named(
    project,
    c("data", "metadata", "events", "event_instruments", "instruments", "repeating_instruments")
  )
})

test_that("pull_redcap_project tolerates optional API failures but not required failures", {
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(redcap_uri, token, verbose, ...) {
      list(data = tibble::tibble(record_id = "1"))
    },
    redcap_metadata_read = function(redcap_uri, token, verbose, ...) {
      list(data = tibble::tibble(
        field_name = "record_id",
        form_name = "main",
        field_type = "text",
        field_label = "Record ID"
      ))
    },
    redcap_event_read = function(...) {
      stop("optional events failed")
    },
    redcap_event_instruments = function(...) {
      stop("optional mapping failed")
    },
    redcap_instruments = function(...) {
      stop("optional instruments failed")
    },
    redcap_instrument_repeating = function(...) {
      stop("optional repeating failed")
    }
  )

  project <- pull_redcap_project(
    redcap_uri = "https://redcap.example/api/",
    token = "test-token"
  )

  expect_equal(nrow(project$data), 1)
  expect_equal(project$events, tibble::tibble())
  expect_equal(project$event_instruments, tibble::tibble())
  expect_equal(project$instruments, tibble::tibble())
  expect_equal(project$repeating_instruments, tibble::tibble())

  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(...) {
      stop("required records failed")
    }
  )

  expect_error(
    pull_redcap_project(
      redcap_uri = "https://redcap.example/api/",
      token = "test-token"
    ),
    "required records failed"
  )
})

test_that("build_quality_report returns expected findings and summaries for API data", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "2", "3"),
    age = c(10, 999, 5, NA),
    consent_date = c("2025-01-01", "2999-01-01", "2025-02-01", ""),
    notes = c("reviewed", "", "ok", "follow up"),
    symptoms___none = c(1, 1, 0, 0),
    symptoms___fever = c(0, 1, 0, 1),
    demographics_complete = c(2, 1, 2, 1),
    redcap_event_name = rep("baseline_arm_1", 4)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "age", "consent_date", "notes", "symptoms"),
    form_name = rep("demographics", 5),
    field_type = c("text", "text", "text", "notes", "checkbox"),
    field_label = c("Record ID", "Age", "Consent Date", "Comments", "Symptoms"),
    select_choices_or_calculations = c(NA, NA, NA, NA, "none, None | fever, Fever"),
    text_validation_type_or_show_slider_number = c(NA, "integer", "date_ymd", NA, NA),
    text_validation_min = c(NA, 0, NA, NA, NA),
    text_validation_max = c(NA, 120, NA, NA, NA),
    required_field = c("y", "y", "n", "n", "n"),
    branching_logic = c(NA, NA, "[missing_field] = '1'", NA, NA),
    identifier = NA
  )

  report <- build_mock_quality_report(redcap_data, metadata)

  expect_s3_class(report, "redcap_quality_report")
  expect_named(report, c("findings", "summaries", "metadata"))
  expect_named(report$summaries, c("project", "forms", "fields", "records", "events"))
  expect_named(
    report$metadata,
    c(
      "fields",
      "forms",
      "events",
      "event_instruments",
      "instruments",
      "repeating_instruments",
      "instrument_structure",
      "choices",
      "validation",
      "branching",
      "record_id_field",
      "source"
    )
  )

  expect_equal(report$summaries$project$source, "api")
  expect_equal(report$summaries$project$record_count, 3)
  expect_equal(report$summaries$project$raw_row_count, 4)
  expect_equal(report$metadata$record_id_field, "record_id")
  expect_equal(report$metadata$source, "api")

  record_findings <- report$findings |>
    dplyr::filter(.data$scope %in% c("record", "record_field"))
  expect_false(any(is.na(record_findings$record_id) | record_findings$record_id == ""))
})

test_that("API metadata checks report label, branching, and text risks", {
  redcap_data <- tibble::tibble(
    record_id = "1",
    field_a = "1",
    field_b = "2",
    notes = "free text",
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "field_a", "field_b", "notes"),
    form_name = "main",
    field_type = c("text", "text", "radio", "notes"),
    field_label = c("Record ID", "Duplicate Label", "Duplicate Label", "Notes"),
    required_field = c("y", "n", "n", "n"),
    branching_logic = c(NA, NA, "[missing_field] = '1'", NA),
    select_choices_or_calculations = c(NA, NA, NA, NA)
  )

  report <- build_mock_quality_report(
    redcap_data,
    metadata,
    checks = "metadata"
  )

  expect_true("duplicate_field_label" %in% report$findings$issue)
  expect_true("missing_choice_definition" %in% report$findings$issue)
  expect_true("orphaned_branching_reference" %in% report$findings$issue)
  expect_true("high_risk_free_text" %in% report$findings$issue)
})

test_that("API metadata aliases are standardized", {
  redcap_data <- tibble::tibble(
    record_id = "1",
    choice_field = "1",
    main_complete = 2
  )

  metadata <- tibble::tibble(
    `Variable / Field Name` = c("record_id", "choice_field"),
    `Form Name` = c("main", "main"),
    `Field Type` = c("text", "radio"),
    `Field Label` = c("Record ID", "Choice Field"),
    `Choices, Calculations, OR Slider Labels` = c(NA, "1, One | 2, Two"),
    `Text Validation Type OR Show Slider Number` = c(NA, NA),
    `Text Validation Min` = c(NA, NA),
    `Text Validation Max` = c(NA, NA),
    `Required Field?` = c("y", "n"),
    `Branching Logic (Show field only if...)` = c(NA, "[record_id] <> 'missing'"),
    `Identifier?` = c(NA, NA)
  )

  report <- build_mock_quality_report(
    redcap_data,
    metadata,
    checks = "metadata"
  )

  expect_equal(report$metadata$fields$field_name, c("record_id", "choice_field"))
  expect_equal(report$metadata$fields$required_field, c(TRUE, FALSE))
  expect_equal(report$metadata$choices$field_name, c("choice_field", "choice_field"))
  expect_equal(report$metadata$branching$referenced_fields, "record_id")
})

test_that("API outlier checks report validation, IQR, and future dates", {
  redcap_data <- tibble::tibble(
    record_id = as.character(seq_len(6)),
    age = c(10, 12, 11, 13, 12, 999),
    consent_date = c(rep("2025-01-01", 5), "2999-01-01"),
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "age", "consent_date"),
    form_name = "main",
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Age", "Consent Date"),
    required_field = c("y", "n", "n"),
    text_validation_type_or_show_slider_number = c(NA, "integer", "date_ymd"),
    text_validation_min = c(NA, 0, NA),
    text_validation_max = c(NA, 120, NA)
  )

  report <- build_mock_quality_report(
    redcap_data,
    metadata,
    checks = "outliers"
  )

  expect_true("outside_validation_range" %in% report$findings$issue)
  expect_true("numeric_iqr_outlier" %in% report$findings$issue)
  expect_true("future_date" %in% report$findings$issue)
})

test_that("API outlier checks ignore invalid date strings", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    visit_date = c("not-a-date", "2025-01-01"),
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "visit_date"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Visit Date"),
    required_field = c("y", "n"),
    text_validation_type_or_show_slider_number = c(NA, "date_ymd")
  )

  expect_no_error(
    report <- build_mock_quality_report(
      redcap_data,
      metadata,
      checks = "outliers"
    )
  )
  expect_false("future_date" %in% report$findings$issue)
})

test_that("API reports drop descriptive text fields before analysis", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    instructions = c(NA, NA),
    required_value = c("present", "")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "instructions", "required_value"),
    form_name = c("main", "main", "main"),
    field_type = c("text", "descriptive", "text"),
    field_label = c("Record ID", "", "Required Value"),
    required_field = c("y", "n", "y")
  )

  expect_warning(
    report <- build_mock_quality_report(redcap_data, metadata),
    NA
  )

  expect_false("instructions" %in% report$metadata$fields$field_name)
  expect_false("instructions" %in% report$summaries$fields$field_name)
  expect_equal(report$summaries$project$field_count, 2)
  expect_equal(report$summaries$forms$field_count, 2)
  expect_false(any(report$findings$field_name == "instructions", na.rm = TRUE))
})

test_that("API field summaries ignore structural missingness from repeating instruments", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_repeat_instrument = c(NA, "repeating_form", "repeating_form"),
    redcap_repeat_instance = c(NA, 1, 2),
    main_value = c("present", NA, NA),
    main_form_complete = c(2, NA, NA),
    repeat_value = c(NA, "first", "second"),
    repeating_form_complete = c(NA, 2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "repeat_value"),
    form_name = c("main_form", "main_form", "repeating_form"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Repeat Value"),
    required_field = c("y", "y", "y")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(report$summaries$project$missing_rate, 0)
  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name %in% c("main_value", "repeat_value")) |>
      dplyr::pull(.data$missing_rate),
    c(0, 0)
  )
  expect_equal(report$summaries$records$missing_field_count, c(0, 0, 0))
})

test_that("API project and form missing rates are weighted missing fractions", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3", "4"),
    gate = c(1, 1, 2, 2),
    always_value = c(NA, "present", "present", "present"),
    conditional_value = c(NA, NA, NA, "present"),
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "gate", "always_value", "conditional_value"),
    form_name = c("main", "main", "main", "main"),
    field_type = c("text", "text", "text", "text"),
    field_label = c("Record ID", "Gate", "Always Value", "Conditional Value"),
    required_field = c("y", "n", "y", "y"),
    branching_logic = c(NA, NA, NA, "[gate] = '2'")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_false("mean_missing_rate" %in% names(report$summaries$project))
  expect_false("mean_missing_rate" %in% names(report$summaries$forms))
  expect_equal(report$summaries$project$missing_rate, 1 / 7)
  expect_equal(report$summaries$forms$missing_rate, 1 / 7)
})

test_that("API operational checks ignore structural completion blanks from repeating instruments", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_repeat_instrument = c(NA, "repeating_form", "repeating_form"),
    redcap_repeat_instance = c(NA, 1, 2),
    main_value = c("present", NA, NA),
    main_form_complete = c(2, NA, NA),
    repeat_value = c(NA, "first", "second"),
    repeating_form_complete = c(NA, 2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "repeat_value"),
    form_name = c("main_form", "main_form", "repeating_form"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Repeat Value"),
    required_field = c("y", "n", "n")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "operational"
  )

  expect_equal(nrow(report$findings), 0)
})

test_that("build_quality_report progress display does not affect report contents", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    required_value = c("present", ""),
    main_complete = c(2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  none_report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    progress = "none"
  )
  utils::capture.output(
    utils::capture.output(
      show_report <- build_mock_quality_report(
        data = redcap_data,
        metadata = metadata,
        progress = "show"
      ),
      type = "message"
    )
  )

  expect_equal(show_report, none_report)
})

test_that("API checkbox consistency findings use REDCap checkbox columns", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    symptoms___none = c(1, 0),
    symptoms___fever = c(1, 1)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("main", "main"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    select_choices_or_calculations = c(NA, "none, None | fever, Fever"),
    required_field = c("y", "n")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "consistency"
  )

  expect_equal(nrow(report$findings), 1)
  expect_equal(report$findings$issue, "checkbox_none_with_other")
  expect_equal(report$findings$record_id, "1")
  expect_equal(report$findings$field_name, "symptoms")
  expect_equal(report$findings$value, "symptoms___none")
})

test_that("API checkbox consistency findings include repeating instance context", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_repeat_instrument = c(NA, "symptom_log", "symptom_log"),
    redcap_repeat_instance = c(NA, 1, 2),
    baseline_value = c("present", NA, NA),
    baseline_complete = c(2, NA, NA),
    symptoms___none = c(NA, 1, 0),
    symptoms___fever = c(NA, 1, 1),
    symptom_log_complete = c(NA, 2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "baseline_value", "symptoms"),
    form_name = c("baseline", "baseline", "symptom_log"),
    field_type = c("text", "text", "checkbox"),
    field_label = c("Record ID", "Baseline", "Symptoms"),
    select_choices_or_calculations = c(NA, NA, "none, None | fever, Fever"),
    required_field = c("y", "n", "n")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "consistency"
  )

  expect_equal(nrow(report$findings), 1)
  expect_equal(report$findings$record_id, "1")
  expect_equal(report$findings$form_name, "symptom_log")
  expect_equal(report$findings$repeat_instrument, "symptom_log")
  expect_equal(report$findings$repeat_instance, "1")
})

test_that("API duplicate checkbox labels use parent field names", {
  redcap_data <- tibble::tibble(
    record_id = "1",
    survey_checkbox___one = 1,
    survey_checkbox___two = 0,
    repeatsurvey_checkbox___one = 1,
    repeatsurvey_checkbox___two = 0
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "survey_checkbox", "repeatsurvey_checkbox"),
    form_name = c("survey", "survey", "repeat_survey"),
    field_type = c("text", "checkbox", "checkbox"),
    field_label = c("Record ID", "Checkbox Field:", "Checkbox Field:"),
    select_choices_or_calculations = c(
      NA,
      "one, Choice 1 | two, Choice 2",
      "one, Choice 1 | two, Choice 2"
    ),
    required_field = c("y", "n", "n")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "metadata"
  )

  duplicate_label <- report$findings |>
    dplyr::filter(.data$issue == "duplicate_field_label")

  expect_equal(nrow(duplicate_label), 1)
  expect_equal(
    duplicate_label$field_name,
    "repeatsurvey_checkbox, survey_checkbox"
  )
  expect_equal(duplicate_label$value, "Checkbox Field:")
  expect_false(grepl("___", duplicate_label$field_name))
})

test_that("API empty-but-valid input returns stable empty report", {
  redcap_data <- tibble::tibble(
    record_id = character(),
    required_value = character(),
    main_complete = numeric()
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  report <- build_mock_quality_report(redcap_data, metadata)

  expect_s3_class(report, "redcap_quality_report")
  expect_equal(nrow(report$findings), 0)
  expect_equal(report$summaries$project$record_count, 0)
  expect_equal(report$summaries$project$raw_row_count, 0)
  expect_equal(report$summaries$project$field_count, 2)
  expect_true(is.na(report$summaries$project$missing_rate))
  expect_true(is.na(report$summaries$forms$missing_rate))
})

test_that("API nonrepeating form status findings include accessible incomplete rows", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    demographic_value = c("present", "present"),
    lab_value = c("present", NA),
    demographics_complete = c(2, 2),
    labs_complete = c(2, 0)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "demographic_value", "lab_value"),
    form_name = c("demographics", "demographics", "labs"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Demographic Value", "Lab Value"),
    required_field = c("y", "n", "n")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "operational"
  )

  expect_equal(nrow(report$findings), 1)
  expect_equal(report$findings$record_id, "2")
  expect_equal(report$findings$form_name, "labs")
  expect_equal(report$findings$field_name, "labs_complete")
  expect_equal(report$findings$value, "Incomplete")
})

test_that("API form applicability excludes forms disabled for a longitudinal event", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1"),
    redcap_event_name = c("event_1_arm_1", "event_2_arm_1"),
    main_value = c("present", "present"),
    lab_value = c(NA, NA),
    main_complete = c(2, 2),
    labs_complete = c(1, 0)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "lab_value"),
    form_name = c("main", "main", "labs"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Lab Value"),
    required_field = c("y", "n", "y")
  )

  event_instruments <- tibble::tibble(
    arm_num = c(1L, 1L, 1L),
    unique_event_name = c("event_1_arm_1", "event_2_arm_1", "event_1_arm_1"),
    form = c("main", "main", "labs")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    event_instruments = event_instruments,
    checks = c("missingness", "operational")
  )

  expect_false(any(
    report$findings$form_name == "labs" &
      report$findings$event_name == "event_2_arm_1",
    na.rm = TRUE
  ))
  expect_true(any(
    report$findings$issue == "required_field_missing" &
      report$findings$form_name == "labs" &
      report$findings$event_name == "event_1_arm_1"
  ))
  expect_true(any(
    report$findings$issue == "incomplete_form_status" &
      report$findings$form_name == "labs" &
      report$findings$event_name == "event_1_arm_1"
  ))
})

test_that("API form applicability excludes Forms Display Logic structural blanks", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    main_value = c("present", "present"),
    conditional_value = c("answered", "structural value"),
    main_complete = c(2, 2),
    conditional_complete = c(2, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "conditional_value"),
    form_name = c("main", "main", "conditional"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Conditional Value"),
    required_field = c("y", "n", "y")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = c("missingness", "operational")
  )

  expect_false(any(
    report$findings$check == "missingness" &
      report$findings$record_id == "2" &
      report$findings$form_name == "conditional"
  ))
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "conditional_value") |>
      dplyr::pull(.data$record_count),
    1
  )
  expect_equal(report$summaries$project$missing_rate, 0)
  expect_equal(report$summaries$records$missing_field_count, c(0, 0))
})

test_that("API missingness requires assessed form status", {
  redcap_data <- tibble::tibble(
    record_id = as.character(1:8),
    conditional_value = rep(NA_character_, 8),
    conditional_complete = c(NA, "", 0, 1, 2, "Incomplete", "Unverified", "Complete")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "conditional_value"),
    form_name = c("conditional", "conditional"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Conditional Value"),
    required_field = c("y", "y")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness",
    sparse_threshold = 0.5
  )

  required_findings <- report$findings |>
    dplyr::filter(.data$issue == "required_field_missing")
  sparse_findings <- report$findings |>
    dplyr::filter(.data$issue == "unexpected_sparse_field")

  expect_equal(required_findings$record_id, c("4", "5", "7", "8"))
  expect_equal(sparse_findings$field_name, "conditional_value")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "conditional_value") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 4L, missing_count = 4L, missing_rate = 1)
  )
  expect_equal(report$summaries$project$missing_rate, 0.5)
  expect_equal(report$summaries$forms$missing_rate, 0.5)
  expect_equal(report$summaries$records$missing_field_count, c(0, 0, 0, 1, 1, 0, 1, 1))
})

test_that("API missingness excludes forms without status columns", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    required_value = c(NA, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  report <- build_mock_quality_report(
    redcap_data,
    metadata,
    checks = "missingness"
  )

  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "required_value") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 0L, missing_count = 0L, missing_rate = NA_real_)
  )
  expect_equal(report$summaries$records$missing_field_count, c(0, 0))
})

test_that("API missingness ignores unchecked checkbox exports on incomplete forms", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    symptoms___fever = c(0, 0),
    symptoms___rash = c(0, 0),
    detail = c(NA, NA),
    symptoms_complete = c(0, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms", "detail"),
    form_name = c("symptoms", "symptoms", "symptoms"),
    field_type = c("text", "checkbox", "text"),
    field_label = c("Record ID", "Symptoms", "Detail"),
    required_field = c("y", "y", "y"),
    select_choices_or_calculations = c(NA, "fever, Fever | rash, Rash", NA)
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(
    report$findings |>
      dplyr::filter(.data$issue == "required_field_missing") |>
      dplyr::select("record_id", "field_name"),
    tibble::tibble(
      record_id = c("2", "2"),
      field_name = c("symptoms", "detail")
    )
  )
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "symptoms") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 1L, missing_count = 1L, missing_rate = 1)
  )
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "detail") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 1L, missing_count = 1L, missing_rate = 1)
  )
})

test_that("API missingness treats checked checkbox options as observed", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    symptoms___fever = c(1, 0),
    symptoms___rash = c(0, 0),
    symptoms_complete = c(2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("symptoms", "symptoms"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    required_field = c("y", "y"),
    select_choices_or_calculations = c(NA, "fever, Fever | rash, Rash")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(
    report$findings |>
      dplyr::filter(.data$issue == "required_field_missing") |>
      dplyr::select("record_id", "field_name"),
    tibble::tibble(record_id = "2", field_name = "symptoms")
  )
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "symptoms") |>
      dplyr::select("record_count", "missing_count", "observed_count", "missing_rate"),
    tibble::tibble(
      record_count = 2L,
      missing_count = 1L,
      observed_count = 1L,
      missing_rate = 0.5
    )
  )
})

test_that("API field summaries ignore structural missingness between repeating instruments", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_repeat_instrument = c(NA, "repeat_a", "repeat_b"),
    redcap_repeat_instance = c(NA, 1, 1),
    main_value = c("present", NA, NA),
    repeat_a_value = c(NA, "a", NA),
    repeat_b_value = c(NA, NA, "b"),
    main_complete = c(2, NA, NA),
    repeat_a_complete = c(NA, 2, NA),
    repeat_b_complete = c(NA, NA, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "repeat_a_value", "repeat_b_value"),
    form_name = c("main", "main", "repeat_a", "repeat_b"),
    field_type = c("text", "text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Repeat A Value", "Repeat B Value"),
    required_field = c("y", "y", "y", "y")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name %in% c("repeat_a_value", "repeat_b_value")) |>
      dplyr::pull(.data$record_count),
    c(1, 1)
  )
  expect_equal(report$summaries$records$missing_field_count, c(0, 0, 0))
})

test_that("API repeating missingness requires matching non-missing form status", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_repeat_instrument = c(NA, "repeat_a", "repeat_a"),
    redcap_repeat_instance = c(NA, 1, 2),
    main_value = c("present", NA, NA),
    repeat_a_value = c(NA, NA, NA),
    main_complete = c(2, NA, NA),
    repeat_a_complete = c(NA, NA, 1)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "main_value", "repeat_a_value"),
    form_name = c("main", "main", "repeat_a"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Main Value", "Repeat A Value"),
    required_field = c("y", "y", "y")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  repeat_findings <- report$findings |>
    dplyr::filter(
      .data$issue == "required_field_missing",
      .data$field_name == "repeat_a_value"
    )

  expect_equal(repeat_findings$repeat_instance, "2")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "repeat_a_value") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 1L, missing_count = 1L, missing_rate = 1)
  )
})

test_that("API completion findings preserve form order around repeating forms", {
  raw_data <- tibble::tibble(
    record_id = c("1", "1", "1", "2", "3"),
    redcap_repeat_instrument = c(NA, "repeated", "repeated", NA, "repeated"),
    redcap_repeat_instance = c(NA, 1, 2, NA, 1),
    baseline_value = c("a", NA, NA, "b", NA),
    repeated_value = c(NA, "x", "y", NA, "z"),
    followup_value = c("missing", NA, NA, "missing", NA),
    baseline_complete = c(2, NA, NA, 2, NA),
    repeated_complete = c(NA, 0, 0, NA, 0),
    followup_complete = c(0, NA, NA, 1, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "baseline_value", "repeated_value", "followup_value"),
    form_name = c("baseline", "baseline", "repeated", "followup"),
    field_type = c("text", "text", "text", "text"),
    field_label = c("Record ID", "Baseline", "Repeated", "Followup"),
    required_field = c("y", "n", "n", "n")
  )

  report <- build_mock_quality_report(
    data = raw_data,
    metadata = metadata,
    checks = "operational"
  )

  findings <- report$findings |>
    dplyr::select(
      "record_id",
      "form_name",
      "event_name",
      "field_name",
      "value"
    )

  expected <- tibble::tibble(
    record_id = c("1", "1", "3", "1", "2"),
    form_name = c("repeated", "repeated", "repeated", "followup", "followup"),
    event_name = rep(NA_character_, 5),
    field_name = c(
      "repeated_complete",
      "repeated_complete",
      "repeated_complete",
      "followup_complete",
      "followup_complete"
    ),
    value = c("Incomplete", "Incomplete", "Incomplete", "Incomplete", "Unverified")
  )

  expect_equal(findings, expected)
})

test_that("API longitudinal completion findings preserve multi-arm event grid", {
  raw_data <- tibble::tibble(
    record_id = c(
      "record_1",
      "record_1",
      "record_2",
      "record_2",
      "record_3",
      "record_3",
      "record_4",
      "record_4"
    ),
    redcap_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_2",
      "event_3_arm_2"
    ),
    nonrepeat_1 = NA_character_,
    nonrepeat_3 = NA_character_,
    nonrepeated_complete = 0,
    nonrepeated2_complete = c(0, NA, 0, NA, 0, NA, NA, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "nonrepeat_1", "nonrepeat_3"),
    form_name = c("nonrepeated", "nonrepeated", "nonrepeated2"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Text Box Input", "Test data"),
    required_field = c("y", "n", "n")
  )

  report <- build_mock_quality_report(
    data = raw_data,
    metadata = metadata,
    checks = "operational"
  )

  findings <- report$findings |>
    dplyr::select("record_id", "form_name", "event_name", "field_name", "value")

  expect_equal(nrow(findings), 11)
  expect_equal(
    findings$event_name[1:8],
    raw_data$redcap_event_name
  )
  expect_equal(
    findings$event_name[9:11],
    raw_data$redcap_event_name[c(1, 3, 5)]
  )
  expect_false(any(is.na(findings$value)))
})

test_that("API event metadata does not override raw event row counts", {
  raw_data <- tibble::tibble(
    record_id = c("record_1", "record_1", "record_2", "record_2"),
    redcap_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_2",
      "event_3_arm_2"
    ),
    nonrepeat_1 = NA_character_,
    nonrepeated_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "nonrepeat_1"),
    form_name = c("nonrepeated", "nonrepeated"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Text Box Input"),
    required_field = c("y", "n")
  )

  api_events <- tibble::tibble(
    event_name = c("Event 1", "Event 2", "Event 1", "Event 3"),
    arm_num = c(1L, 1L, 2L, 2L),
    unique_event_name = c(
      "event_1_arm_1",
      "event_2_arm_1",
      "event_1_arm_2",
      "event_3_arm_2"
    ),
    custom_event_label = NA_character_,
    event_id = 1:4
  )

  report <- build_mock_quality_report(
    data = raw_data,
    metadata = metadata,
    events = api_events
  )

  expect_equal(report$summaries$project$event_count, 4)
  expect_equal(
    sort(report$summaries$events$event_name),
    sort(raw_data$redcap_event_name)
  )
  expect_equal(report$metadata$events$redcap_event_name, api_events$unique_event_name)
  expect_equal(report$metadata$events$redcap_arm, as.character(api_events$arm_num))
})

test_that("API completion status values are normalized", {
  raw_data <- tibble::tibble(
    record_id = c("1", "2", "3", "4", "5", "6", "7"),
    value = letters[1:7],
    demographics_complete = c(0, 1, 2, NA, "Incomplete", "Unverified", "Complete")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "value"),
    form_name = c("demographics", "demographics"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Value"),
    required_field = c("y", "n")
  )

  report <- build_mock_quality_report(
    data = raw_data,
    metadata = metadata,
    checks = "operational"
  )

  expect_equal(report$findings$record_id, c("1", "2", "4", "5", "6"))
  expect_equal(
    report$findings$value,
    c("Incomplete", "Unverified", NA, "Incomplete", "Unverified")
  )
  expect_equal(report$findings$expected, rep("Complete", 5))
})

test_that("API required missingness respects simple branching logic", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    field_a = c(1, 2, 1),
    field_b = c(NA, NA, "present"),
    main_complete = c(2, 2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "field_a", "field_b"),
    form_name = c("main", "main", "main"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Field A", "Field B"),
    required_field = c("y", "y", "y"),
    branching_logic = c(NA, NA, "[field_a] = '1'")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  field_b_findings <- report$findings |>
    dplyr::filter(
      .data$issue == "required_field_missing",
      .data$field_name == "field_b"
    )

  expect_equal(field_b_findings$record_id, "1")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "field_b") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 2L, missing_count = 1L, missing_rate = 0.5)
  )
  expect_equal(report$summaries$records$missing_field_count, c(1, 0, 0))
})

test_that("API required missingness treats NA branching evaluations as hidden", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    infusion_admin = c(0, 1, NA),
    infusion_reason = c(NA, NA, NA),
    infusion_complete = c(2, 2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "infusion_admin", "infusion_reason"),
    form_name = c("infusion", "infusion", "infusion"),
    field_type = c("text", "yesno", "text"),
    field_label = c("Record ID", "Infusion Administered", "Infusion Reason"),
    required_field = c("y", "y", "y"),
    branching_logic = c(NA, NA, "[infusion_admin] = '0'")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  infusion_findings <- report$findings |>
    dplyr::filter(
      .data$issue == "required_field_missing",
      .data$field_name == "infusion_reason"
    )

  expect_equal(infusion_findings$record_id, "1")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "infusion_reason") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 1L, missing_count = 1L, missing_rate = 1)
  )
})

test_that("API required missingness respects compound branching logic", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3", "4"),
    field_a = c(1, 1, 2, 2),
    field_c = c("yes", "no", "yes", "yes"),
    field_d = c(NA, NA, NA, "present"),
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "field_a", "field_c", "field_d"),
    form_name = c("main", "main", "main", "main"),
    field_type = c("text", "text", "text", "text"),
    field_label = c("Record ID", "Field A", "Field C", "Field D"),
    required_field = c("y", "n", "n", "y"),
    branching_logic = c(NA, NA, NA, "([field_a] = '1' and [field_c] = 'yes') or [field_a] = '2'")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  field_d_findings <- report$findings |>
    dplyr::filter(
      .data$issue == "required_field_missing",
      .data$field_name == "field_d"
    )

  expect_equal(field_d_findings$record_id, c("1", "3"))
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "field_d") |>
      dplyr::pull(.data$record_count),
    3
  )
})

test_that("API required missingness respects inequality and numeric branching logic", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    age = c(6, 4, 7),
    status = c("active", "active", "inactive"),
    followup_detail = c(NA, NA, NA),
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "age", "status", "followup_detail"),
    form_name = c("main", "main", "main", "main"),
    field_type = c("text", "text", "text", "text"),
    field_label = c("Record ID", "Age", "Status", "Followup Detail"),
    required_field = c("y", "n", "n", "y"),
    branching_logic = c(NA, NA, NA, "[age] > 5 and [status] <> 'inactive'")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  followup_findings <- report$findings |>
    dplyr::filter(
      .data$issue == "required_field_missing",
      .data$field_name == "followup_detail"
    )

  expect_equal(followup_findings$record_id, "1")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "followup_detail") |>
      dplyr::pull(.data$record_count),
    1
  )
})

test_that("API required missingness respects checkbox branching logic", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    symptoms___fever = c(1, 0, 1),
    fever_detail = c(NA, NA, "high"),
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms", "fever_detail"),
    form_name = c("main", "main", "main"),
    field_type = c("text", "checkbox", "text"),
    field_label = c("Record ID", "Symptoms", "Fever Detail"),
    select_choices_or_calculations = c(NA, "fever, Fever", NA),
    required_field = c("y", "n", "y"),
    branching_logic = c(NA, NA, "[symptoms(fever)] = '1'")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  fever_findings <- report$findings |>
    dplyr::filter(
      .data$issue == "required_field_missing",
      .data$field_name == "fever_detail"
    )

  expect_equal(fever_findings$record_id, "1")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "fever_detail") |>
      dplyr::pull(.data$record_count),
    2
  )
})

test_that("API required missingness keeps unsupported branching logic assessable", {
  redcap_data <- tibble::tibble(
    record_id = "1",
    start_date = "2025-01-01",
    followup_date = NA_character_,
    main_complete = 2
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "start_date", "followup_date"),
    form_name = c("main", "main", "main"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Start Date", "Followup Date"),
    required_field = c("y", "n", "y"),
    branching_logic = c(NA, NA, "datediff([start_date], 'today', 'd') > 30")
  )

  report <- build_mock_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_true(any(
    report$findings$issue == "required_field_missing" &
      report$findings$field_name == "followup_date"
  ))
})
