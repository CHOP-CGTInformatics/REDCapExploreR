test_that("build_quality_report requires REDCap API credentials", {
  expect_error(
    build_quality_report(progress = "none"),
    "redcap_uri"
  )
  expect_error(
    build_quality_report(redcap_uri = "", token = "", progress = "none"),
    "redcap_uri"
  )
  expect_error(
    build_quality_report(redcap_uri = 1, token = 2, progress = "none"),
    "redcap_uri"
  )
})

test_that("build_quality_report validates scalar finite thresholds", {
  expect_error(
    build_quality_report(sparse_threshold = NA_real_, progress = "none"),
    "sparse_threshold"
  )
  expect_error(
    build_quality_report(outlier_iqr_multiplier = c(1, 2), progress = "none"),
    "outlier_iqr_multiplier"
  )
})

test_that("record import preserves raw choice codes as characters", {
  captured <- NULL
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(..., guess_type, na) {
      captured <<- list(guess_type = guess_type, na = na)
      choice_value <- if (guess_type) FALSE else "1"
      list(
        data = tibble::tibble(
          record_id = "1",
          choice_field = choice_value
        )
      )
    },
    redcap_metadata_read = function(...) {
      list(
        data = tibble::tibble(
          field_name = c("record_id", "choice_field"),
          form_name = "main",
          field_type = c("text", "dropdown"),
          field_label = c("Record ID", "Choice"),
          select_choices_or_calculations = c(NA, "1, One | 2, Two")
        )
      )
    },
    redcap_event_read = function(...) list(data = tibble::tibble()),
    redcap_event_instruments = function(...) list(data = tibble::tibble()),
    redcap_instruments = function(...) list(data = tibble::tibble()),
    redcap_instrument_repeating = function(...) list(data = tibble::tibble())
  )

  project <- pull_redcap_project(
    redcap_uri = "https://redcap.example/api/",
    token = "test-token"
  )
  report <- get_quality_report(
    project = get_quality_project_data(project),
    checks = "consistency",
    sparse_threshold = 0.95,
    outlier_iqr_multiplier = 3
  )

  expect_false(captured$guess_type)
  expect_equal(captured$na, "")
  expect_equal(project$data$choice_field, "1")
  expect_equal(nrow(report$findings), 0)
})

test_that("repeating configuration does not depend on exported rows", {
  redcap_data <- tibble::tibble(
    record_id = character(),
    repeat_value = character(),
    repeat_form_complete = integer()
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "repeat_value"),
    form_name = c("main", "repeat_form"),
    field_type = "text",
    field_label = c("Record ID", "Repeat Value"),
    required_field = "n"
  )
  repeating_instruments <- tibble::tibble(
    event_name = "Event 1",
    arm_num = 1L,
    unique_event_name = "event_1_arm_1",
    form_name = "repeat_form",
    custom_form_label = NA_character_
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = repeating_instruments,
    checks = "missingness"
  )

  expect_true(report$summaries$project$repeating_enabled)
  expect_equal(
    report$metadata$repeating_instruments$redcap_event_name,
    "event_1_arm_1"
  )
  expect_equal(nrow(report$findings), 0)
})

test_that("build_quality_report returns the public report schema from a raw export", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3", "4"),
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
    select_choices_or_calculations = c(
      NA,
      NA,
      NA,
      NA,
      "none, None | fever, Fever"
    ),
    text_validation_type_or_show_slider_number = c(
      NA,
      "integer",
      "date_ymd",
      NA,
      NA
    ),
    text_validation_min = c(NA, 0, NA, NA, NA),
    text_validation_max = c(NA, 120, NA, NA, NA),
    required_field = c("y", "y", "n", "n", "n"),
    branching_logic = c(NA, NA, "[missing_field] = '1'", NA, NA),
    identifier = NA
  )

  testthat::local_mocked_bindings(
    pull_redcap_project = function(redcap_uri, token) list(data = redcap_data, metadata = metadata)
  )
  report <- build_quality_report("https://example.invalid", "synthetic", progress = "none")

  expect_s3_class(report, "redcap_quality_report")
  expect_named(report, c("findings", "summaries", "metadata"))
  expect_named(
    report$summaries,
    c("project", "forms", "fields", "records", "events")
  )
  expect_named(
    report$metadata,
    c(
      "fields",
      "forms",
      "events",
      "event_instruments",
      "instruments",
      "repeating_instruments",
      "choices",
      "validation",
      "branching",
      "record_id_field",
      "source",
      "settings"
    )
  )

  expect_equal(report$summaries$project$record_count, 4)
  expect_equal(report$summaries$project$raw_row_count, 4)
  expect_equal(report$metadata$record_id_field, "record_id")
  expect_equal(report$metadata$source, "api")

  record_findings <- report$findings |>
    dplyr::filter(.data$scope %in% c("record", "record_field"))
  expect_false(any(
    is.na(record_findings$record_id) | record_findings$record_id == ""
  ))
})

test_that("metadata checks report label, choice, and text risks", {
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

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = "metadata"
  )

  expect_true("duplicate_field_label" %in% report$findings$issue)
  expect_true("missing_choice_definition" %in% report$findings$issue)
  expect_true("high_risk_free_text" %in% report$findings$issue)
})

test_that("metadata checks leave unresolved branching references to relationship reporting", {
  metadata <- get_test_metadata(c("record_id", "dependent"))
  metadata$branching_logic <- c(NA, "[retired_field] = '1'")
  report <- get_test_quality_report(
    tibble::tibble(record_id = "1", dependent = "value", main_complete = "2"), metadata,
    checks = "metadata"
  )
  expect_equal(nrow(report$findings), 0L)
})

test_that("outlier checks report validation, IQR, and future dates", {
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

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = "outliers"
  )

  expect_equal(
    report$findings |> dplyr::select("record_id", "field_name", "issue"),
    tibble::tibble(
      record_id = c("6", "6", "6"),
      field_name = c("age", "age", "consent_date"),
      issue = c("outside_validation_range", "numeric_iqr_outlier", "future_date")
    )
  )
})

test_that("number validation accepts decimals without a leading zero", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    score = c(".850", "-.850", "."),
    main_complete = 2
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "score"),
    form_name = "main",
    field_type = "text",
    field_label = c("Record ID", "Score"),
    text_validation_type_or_show_slider_number = c(NA, "number")
  )

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = "consistency"
  )

  expect_equal(report$findings$issue, "invalid_validation_format")
  expect_equal(report$findings$record_id, "3")
  expect_equal(report$findings$value, ".")
})

test_that("consistency enforces only configured text validations", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    validated_text = c("10", "10.5", ""),
    unvalidated_text = c("anything", "10.5", ""),
    unsupported_text = c("abc", "123", ""),
    non_text = c("10", "10.5", ""),
    main_complete = c(2, 0, NA)
  )
  metadata <- tibble::tibble(
    field_name = c(
      "record_id",
      "validated_text",
      "unvalidated_text",
      "unsupported_text",
      "non_text"
    ),
    form_name = "main",
    field_type = c("text", "text", "text", "text", "notes"),
    field_label = c(
      "Record ID",
      "Validated",
      "Unvalidated",
      "Unsupported",
      "Non-text"
    ),
    text_validation_type_or_show_slider_number = c(
      NA,
      "integer",
      NA,
      "alpha_only",
      NA
    )
  )

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = "consistency"
  )

  expect_equal(report$findings$issue, "invalid_validation_format")
  expect_equal(report$findings$record_id, "2")
  expect_equal(report$findings$field_name, "validated_text")
  expect_equal(report$findings$value, "10.5")
})

test_that("validation bounds use configured type-aware metadata", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    score = c("4", "11", "invalid"),
    comma_score = c("1,4", "2,6", "invalid"),
    visit_date = c("2024-12-31", "2025-06-01", "invalid"),
    visit_datetime = c(
      "2025-01-01 08:59",
      "2025-01-01 09:00",
      "invalid"
    ),
    visit_time = c("18:01", "18:00", "invalid"),
    main_complete = c(2, 0, NA)
  )
  metadata <- tibble::tibble(
    field_name = c(
      "record_id",
      "score",
      "comma_score",
      "visit_date",
      "visit_datetime",
      "visit_time"
    ),
    form_name = "main",
    field_type = "text",
    field_label = c(
      "Record ID",
      "Score",
      "Comma Score",
      "Visit Date",
      "Visit Datetime",
      "Visit Time"
    ),
    text_validation_type_or_show_slider_number = c(
      NA,
      "integer",
      "number_1dp_comma_decimal",
      "date_ymd",
      "datetime_ymd",
      "time"
    ),
    text_validation_min = c(
      NA,
      "5",
      "1,5",
      "2025-01-01",
      "2025-01-01 09:00",
      NA
    ),
    text_validation_max = c(NA, "10", "2,5", NA, NA, "18:00")
  )

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = c("outliers", "consistency")
  )
  range_findings <- report$findings |>
    dplyr::filter(.data$issue == "outside_validation_range")
  format_findings <- report$findings |>
    dplyr::filter(.data$issue == "invalid_validation_format")

  expect_equal(
    range_findings |> dplyr::select("record_id", "field_name", "expected"),
    tibble::tribble(
      ~record_id, ~field_name, ~expected,
      "1", "score", "Between 5 and 10",
      "2", "score", "Between 5 and 10",
      "1", "comma_score", "Between 1,5 and 2,5",
      "2", "comma_score", "Between 1,5 and 2,5",
      "1", "visit_date", "At least 2025-01-01",
      "1", "visit_datetime", "At least 2025-01-01 09:00",
      "1", "visit_time", "At most 18:00"
    )
  )
  expect_equal(
    format_findings |> dplyr::select("record_id", "field_name"),
    tibble::tibble(
      record_id = rep("3", 5),
      field_name = c("score", "comma_score", "visit_date", "visit_datetime", "visit_time")
    )
  )
})

test_that("malformed dates do not also produce future findings", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    visit_date = c("2999-01-01", "2999-13-01"),
    main_complete = c(2, 0)
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "visit_date"),
    form_name = "main",
    field_type = "text",
    field_label = c("Record ID", "Visit Date"),
    text_validation_type_or_show_slider_number = c(NA, "date_ymd")
  )

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = c("outliers", "consistency")
  )

  expect_equal(
    report$findings$record_id[report$findings$issue == "future_date"],
    "1"
  )
  expect_equal(
    report$findings$record_id[
      report$findings$issue == "invalid_validation_format"
    ],
    "2"
  )
})

test_that("reports drop descriptive text fields before analysis", {
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
    report <- get_test_quality_report(redcap_data, metadata),
    NA
  )

  expect_false("instructions" %in% report$metadata$fields$field_name)
  expect_false("instructions" %in% report$summaries$fields$field_name)
  expect_equal(report$summaries$project$field_count, 2)
  expect_equal(report$summaries$forms$field_count, 2)
  expect_false(any(report$findings$field_name == "instructions", na.rm = TRUE))
})

test_that("field summaries ignore structural missingness from repeating instruments", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(form_name = "repeating_form"),
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
  expect_equal(report$summaries$records$missing_field_count, 0)
})

test_that("project and form missing rates are weighted missing fractions", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_false("mean_missing_rate" %in% names(report$summaries$project))
  expect_false("mean_missing_rate" %in% names(report$summaries$forms))
  expect_equal(report$summaries$project$missing_rate, 1 / 7)
  expect_equal(report$summaries$forms$missing_rate, 1 / 7)
})

test_that("operational checks ignore structural completion blanks from repeating instruments", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(form_name = "repeating_form"),
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

  testthat::local_mocked_bindings(
    pull_redcap_project = function(redcap_uri, token) {
      expect_equal(c(redcap_uri, token), c("https://example.invalid", "synthetic"))
      list(data = redcap_data, metadata = metadata)
    }
  )
  none_report <- build_quality_report("https://example.invalid", "synthetic", progress = "none")
  utils::capture.output(
    utils::capture.output(
      show_report <- build_quality_report("https://example.invalid", "synthetic", progress = "show"),
      type = "message"
    )
  )

  expect_equal(show_report, none_report)
})

test_that("checkbox consistency findings use REDCap checkbox columns", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    symptoms___none = c(1, 0, 1),
    symptoms___fever = c(1, 1, 1),
    main_complete = c(2, 2, 0)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("main", "main"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    select_choices_or_calculations = c(NA, "none, None | fever, Fever"),
    required_field = c("y", "n")
  )

  report <- get_test_quality_report(
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

test_that("consistency detects invalid non-checkbox choice values", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    radio_field = c("1", "9", ""),
    dropdown_field = c("a", "z", NA),
    yesno_field = c("1", "2", "0"),
    truefalse_field = c("0", "true", "1"),
    checkbox_field___a = c(1, 1, 0),
    checkbox_field___b = c(0, 0, 0),
    undefined_field = c("1", "9", ""),
    main_complete = c(2, 0, NA)
  )
  metadata <- tibble::tibble(
    field_name = c(
      "record_id",
      "radio_field",
      "dropdown_field",
      "yesno_field",
      "truefalse_field",
      "checkbox_field",
      "undefined_field"
    ),
    form_name = "main",
    field_type = c(
      "text",
      "radio",
      "dropdown",
      "yesno",
      "truefalse",
      "checkbox",
      "radio"
    ),
    field_label = c(
      "Record ID",
      "Radio",
      "Dropdown",
      "Yes/No",
      "True/False",
      "Checkbox",
      "Undefined"
    ),
    select_choices_or_calculations = c(
      NA,
      "1, One | 2, Two",
      "a, A | b, B",
      NA,
      NA,
      "A, A | B, B",
      NA
    )
  )

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = "consistency"
  )
  findings <- report$findings |>
    dplyr::filter(.data$issue == "invalid_choice_value")

  expect_equal(nrow(findings), 4)
  expect_true(all(findings$record_id == "2"))
  expect_setequal(
    findings$field_name,
    c(
      "radio_field",
      "dropdown_field",
      "yesno_field",
      "truefalse_field"
    )
  )
  expect_false("checkbox_field" %in% findings$field_name)
  expect_false("undefined_field" %in% findings$field_name)
})

test_that("invalid choices include applicable repeating context", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "1"),
    redcap_event_name = "event_1_arm_1",
    redcap_repeat_instrument = c(NA, "repeat_form", "other_form"),
    redcap_repeat_instance = c(NA, 1, 1),
    repeat_choice = c(NA, "9", "9"),
    repeat_form_complete = c(NA, 0, NA)
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "repeat_choice"),
    form_name = c("main", "repeat_form"),
    field_type = c("text", "radio"),
    field_label = c("Record ID", "Repeat Choice"),
    select_choices_or_calculations = c(NA, "1, One | 2, Two")
  )

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    repeating_instruments = tibble::tibble(form_name = "repeat_form"),
    checks = "consistency"
  )

  expect_equal(report$findings$issue, "invalid_choice_value")
  expect_equal(report$findings$event_name, "event_1_arm_1")
  expect_equal(report$findings$repeat_instrument, "repeat_form")
  expect_equal(report$findings$repeat_instance, "1")
})

test_that("checkbox consistency reports fields with no selected options", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    emergency___only = c(0, 0),
    symptoms___fever = c(0, 1),
    unavailable___flag = c(NA, NA),
    main_complete = c(2, 1)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "emergency", "symptoms", "unavailable"),
    form_name = "main",
    field_type = c("text", "checkbox", "checkbox", "checkbox"),
    field_label = c(
      "Record ID",
      "Only Check In Case of Emergency",
      "Symptoms",
      "Unavailable Flag"
    ),
    select_choices_or_calculations = c(
      NA,
      "only, Only Check In Case of Emergency",
      "fever, Fever",
      "flag, Flag"
    ),
    required_field = "n"
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "consistency"
  )

  expect_equal(nrow(report$findings), 1)
  expect_equal(report$findings$issue, "checkbox_no_values_selected")
  expect_equal(report$findings$severity, "info")
  expect_equal(report$findings$scope, "field")
  expect_true(is.na(report$findings$record_id))
  expect_equal(report$findings$field_name, "emergency")
})

test_that("checkbox consistency excludes unassessed form statuses", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    emergency___only = c(0, 0),
    main_complete = c(0, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "emergency"),
    form_name = "main",
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Only Check In Case of Emergency"),
    select_choices_or_calculations = c(
      NA,
      "only, Only Check In Case of Emergency"
    ),
    required_field = "n"
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "consistency"
  )

  expect_equal(nrow(report$findings), 0)
})

test_that("checkbox consistency uses metadata choice values", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    symptoms___99 = c(1, 0),
    symptoms___1 = c(1, 1),
    main_complete = 2
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = "main",
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    select_choices_or_calculations = c(NA, "99, None | 1, Fever")
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "consistency"
  )

  expect_equal(report$findings$record_id, "1")
  expect_equal(report$findings$value, "symptoms___99")
})

test_that("checkbox consistency findings include repeating instance context", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(form_name = "symptom_log"),
    checks = "consistency"
  )

  expect_equal(nrow(report$findings), 1)
  expect_equal(report$findings$record_id, "1")
  expect_equal(report$findings$form_name, "symptom_log")
  expect_equal(report$findings$repeat_instrument, "symptom_log")
  expect_equal(report$findings$repeat_instance, "1")
})

test_that("duplicate checkbox labels use parent field names", {
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

  report <- get_test_quality_report(
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

test_that("empty-but-valid input returns stable empty report", {
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

  report <- get_test_quality_report(redcap_data, metadata)

  expect_s3_class(report, "redcap_quality_report")
  expect_equal(nrow(report$findings), 0)
  expect_equal(report$summaries$project$record_count, 0)
  expect_equal(report$summaries$project$raw_row_count, 0)
  expect_equal(report$summaries$project$field_count, 2)
  expect_true(is.na(report$summaries$project$missing_rate))
  expect_true(is.na(report$summaries$forms$missing_rate))
})

test_that("nonrepeating form status findings include accessible incomplete rows", {
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

  report <- get_test_quality_report(
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

test_that("form applicability excludes forms disabled for a longitudinal event", {
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

  report <- get_test_quality_report(
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

test_that("form applicability excludes Forms Display Logic structural blanks", {
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

  report <- get_test_quality_report(
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

test_that("missingness requires assessed form status", {
  redcap_data <- tibble::tibble(
    record_id = as.character(1:5),
    conditional_value = rep(NA_character_, 5),
    conditional_complete = c(NA, "", "0", "1", "2")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "conditional_value"),
    form_name = c("conditional", "conditional"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Conditional Value"),
    required_field = c("y", "y")
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness",
    sparse_threshold = 0.5
  )

  required_findings <- report$findings |>
    dplyr::filter(.data$issue == "required_field_missing")
  sparse_findings <- report$findings |>
    dplyr::filter(.data$issue == "unexpected_sparse_field")

  expect_equal(required_findings$record_id, c("4", "5"))
  expect_equal(sparse_findings$field_name, "conditional_value")
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "conditional_value") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 2L, missing_count = 2L, missing_rate = 1)
  )
  expect_equal(report$summaries$project$missing_rate, 0.5)
  expect_equal(report$summaries$forms$missing_rate, 0.5)
  expect_equal(
    report$summaries$records$missing_field_count,
    c(0, 0, 0, 1, 1)
  )
})

test_that("missingness excludes forms without status columns", {
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

  report <- get_test_quality_report(
    redcap_data,
    metadata,
    checks = "missingness"
  )

  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "required_value") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(
      record_count = 0L,
      missing_count = 0L,
      missing_rate = NA_real_
    )
  )
  expect_equal(report$summaries$records$missing_field_count, c(0, 0))
})

test_that("missingness treats explicit unchecked checkbox values as observed", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(
    report$findings |>
      dplyr::filter(.data$issue == "required_field_missing") |>
      dplyr::select("record_id", "field_name"),
    tibble::tibble(record_id = "2", field_name = "detail")
  )
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "symptoms") |>
      dplyr::select(
        "record_count",
        "missing_count",
        "observed_count",
        "missing_rate"
      ),
    tibble::tibble(
      record_count = 1L,
      missing_count = 0L,
      observed_count = 1L,
      missing_rate = 0
    )
  )
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "detail") |>
      dplyr::select("record_count", "missing_count", "missing_rate"),
    tibble::tibble(record_count = 1L, missing_count = 1L, missing_rate = 1)
  )
})

test_that("missingness distinguishes checkbox values from unavailable data", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "3"),
    symptoms___fever = c(1, 0, NA),
    symptoms___rash = c(0, 0, NA),
    symptoms_complete = c(2, 2, 2)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("symptoms", "symptoms"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    required_field = c("y", "y"),
    select_choices_or_calculations = c(NA, "fever, Fever | rash, Rash")
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(
    report$findings |>
      dplyr::filter(.data$issue == "required_field_missing") |>
      dplyr::select("record_id", "field_name"),
    tibble::tibble(record_id = "3", field_name = "symptoms")
  )
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name == "symptoms") |>
      dplyr::select(
        "record_count",
        "missing_count",
        "observed_count",
        "missing_rate"
      ),
    tibble::tibble(
      record_count = 3L,
      missing_count = 1L,
      observed_count = 2L,
      missing_rate = 1 / 3
    )
  )
})

test_that("field summaries ignore structural missingness between repeating instruments", {
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
    field_name = c(
      "record_id",
      "main_value",
      "repeat_a_value",
      "repeat_b_value"
    ),
    form_name = c("main", "main", "repeat_a", "repeat_b"),
    field_type = c("text", "text", "text", "text"),
    field_label = c(
      "Record ID",
      "Main Value",
      "Repeat A Value",
      "Repeat B Value"
    ),
    required_field = c("y", "y", "y", "y")
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(
      form_name = c("repeat_a", "repeat_b")
    ),
    checks = "missingness"
  )

  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(
        .data$field_name %in% c("repeat_a_value", "repeat_b_value")
      ) |>
      dplyr::pull(.data$record_count),
    c(1, 1)
  )
  expect_equal(report$summaries$records$missing_field_count, 0)
})

test_that("repeating missingness requires matching non-missing form status", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(form_name = "repeat_a"),
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

test_that("completion findings preserve form order around repeating forms", {
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
    field_name = c(
      "record_id",
      "baseline_value",
      "repeated_value",
      "followup_value"
    ),
    form_name = c("baseline", "baseline", "repeated", "followup"),
    field_type = c("text", "text", "text", "text"),
    field_label = c("Record ID", "Baseline", "Repeated", "Followup"),
    required_field = c("y", "n", "n", "n")
  )

  report <- get_test_quality_report(
    data = raw_data,
    metadata = metadata,
    repeating_instruments = tibble::tibble(form_name = "repeated"),
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
    value = c(
      "Incomplete",
      "Incomplete",
      "Incomplete",
      "Incomplete",
      "Unverified"
    )
  )

  expect_equal(findings, expected)
})

test_that("longitudinal completion findings preserve multi-arm event grid", {
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

  report <- get_test_quality_report(
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

test_that("event metadata does not override raw event row counts", {
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

  report <- get_test_quality_report(
    data = raw_data,
    metadata = metadata,
    events = api_events
  )

  expect_equal(report$summaries$project$event_count, 4)
  expect_equal(
    sort(report$summaries$events$event_name),
    sort(raw_data$redcap_event_name)
  )
  expect_equal(
    report$metadata$events$redcap_event_name,
    api_events$unique_event_name
  )
  expect_equal(
    report$metadata$events$redcap_arm,
    as.character(api_events$arm_num)
  )
})

test_that("required missingness respects simple branching logic", {
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

  report <- get_test_quality_report(
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

test_that("record summaries aggregate longitudinal rows", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "1", "2"),
    redcap_event_name = c("baseline_arm_1", "followup_arm_1", "baseline_arm_1"),
    required_value = c(NA, NA, "present"),
    main_complete = 2
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = "main",
    field_type = "text",
    field_label = c("Record ID", "Required Value"),
    required_field = "y"
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_equal(report$summaries$records$record_id, c("1", "2"))
  expect_equal(report$summaries$records$missing_field_count, c(2, 0))
  expect_equal(report$summaries$records$finding_count, c(2L, 0L))
})

test_that("IQR checks exclude numeric-coded categorical fields", {
  redcap_data <- tibble::tibble(
    record_id = as.character(seq_len(6)),
    category = c(1, 2, 2, 3, 3, 99),
    main_complete = 2
  )
  metadata <- tibble::tibble(
    field_name = c("record_id", "category"),
    form_name = "main",
    field_type = c("text", "radio"),
    field_label = c("Record ID", "Category"),
    select_choices_or_calculations = c(
      NA,
      "1, A | 2, B | 3, C | 99, Other"
    )
  )

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "outliers"
  )

  expect_false("numeric_iqr_outlier" %in% report$findings$issue)
})

test_that("required missingness treats NA branching evaluations as hidden", {
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

  report <- get_test_quality_report(
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

test_that("required missingness respects compound branching logic", {
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
    branching_logic = c(
      NA,
      NA,
      NA,
      "([field_a] = '1' and [field_c] = 'yes') or [field_a] = '2'"
    )
  )

  report <- get_test_quality_report(
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

test_that("required missingness respects inequality and numeric branching logic", {
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

  report <- get_test_quality_report(
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

test_that("required missingness respects checkbox branching logic", {
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

  report <- get_test_quality_report(
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

test_that("required missingness keeps unsupported branching logic assessable", {
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

  report <- get_test_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness"
  )

  expect_true(any(
    report$findings$issue == "required_field_missing" &
      report$findings$field_name == "followup_date"
  ))
})

test_that("IQR uses eligible finite values and keeps assessing the report", {
  data <- tibble::tibble(
    record_id = as.character(1:6), redcap_event_name = c(rep("enabled_arm_1", 5), "disabled_arm_1"),
    value = c("1", "2", "3", "4", "5", "999"), main_complete = "2"
  )
  metadata <- get_test_metadata(c("record_id", "value"))
  report <- get_test_quality_report(data, metadata, "outliers",
    event_instruments = tibble::tibble(unique_event_name = "enabled_arm_1", form = "main")
  )
  expect_equal(nrow(report$findings), 0)
  data <- dplyr::select(data, -"redcap_event_name")
  data$value <- c("1", "2", "3", "4", "Inf", "-Inf")
  expect_warning(report <- get_test_quality_report(data, metadata, "outliers"), "excluded from IQR")
  expect_equal(nrow(report$findings), 0)
  data$value <- c("1", rep("Inf", 5))
  expect_warning(get_test_quality_report(data, metadata, "outliers"), "excluded from IQR")
  expect_true(is.na(get_parsed_validation_values(paste(rep("9", 400), collapse = ""), "number")))
})

test_that("checkbox meaning follows metadata labels rather than legacy codes", {
  data <- tibble::tibble(record_id = "1", support___no = "1", support___vent = "1", main_complete = "2")
  metadata <- get_test_metadata(c("record_id", "support"), types = c("text", "checkbox"))
  metadata$select_choices_or_calculations <- c(NA, "no, Nasal oxygen | vent, Ventilation")
  expect_equal(nrow(get_test_quality_report(data, metadata, "consistency")$findings), 0)
  metadata$select_choices_or_calculations[2] <- "no, None | vent, Ventilation"
  expect_equal(get_test_quality_report(data, metadata, "consistency")$findings$issue, "checkbox_none_with_other")
})

test_that("report presentation exposes settings, assessment coverage and denominators", {
  data <- tibble::tibble(record_id = "1", detail = NA_character_, main_complete = "0")
  metadata <- get_test_metadata(c("record_id", "detail"))
  report <- get_test_quality_report(data, metadata, checks = "missingness")
  expect_output(print(report), "No values were eligible")
  expect_equal(report$metadata$settings$checks, "missingness")
  expect_equal(report$metadata$settings$evaluation_date, as.Date("2026-11-09"))
  data$main_complete <- "2"
  report <- get_test_quality_report(data, metadata, checks = "missingness")
  expect_match(report$findings$message, "1 of 1 assessed values are missing \\(100%\\); threshold: 95%")
  expect_equal(report$metadata$settings$package_version, as.character(utils::packageVersion("REDCapExploreR")))
})
