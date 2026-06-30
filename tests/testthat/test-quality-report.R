test_that("build_quality_report returns expected findings and summaries for raw data", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2", "2", "3"),
    age = c(10, 999, 5, NA),
    consent_date = c("2025-01-01", "2999-01-01", "2025-02-01", ""),
    notes = c("reviewed", "", "ok", "follow up"),
    symptoms___none = c(1, 1, 0, 0),
    symptoms___fever = c(0, 1, 0, 1),
    demographics_complete = c(2, 1, 2, NA),
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

  report <- build_quality_report(redcap_data, metadata)

  expect_s3_class(report, "redcap_quality_report")
  expect_named(report, c("findings", "summaries", "metadata"))
  expect_named(report$summaries, c("project", "forms", "fields", "records", "events"))
  expect_named(
    report$metadata,
    c(
      "fields",
      "forms",
      "events",
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

  expect_equal(report$summaries$project$record_count, 3)
  expect_equal(report$summaries$project$raw_row_count, 4)
  expect_false("supertbl_row_count" %in% names(report$summaries$project))
  expect_equal(report$metadata$record_id_field, "record_id")

  expect_true("required_field_missing" %in% report$findings$issue)
  expect_true("outside_validation_range" %in% report$findings$issue)
  expect_true("future_date" %in% report$findings$issue)
  expect_true("orphaned_branching_reference" %in% report$findings$issue)
  expect_true("high_risk_free_text" %in% report$findings$issue)
  expect_true("incomplete_form_status" %in% report$findings$issue)
  expect_true("checkbox_none_with_other" %in% report$findings$issue)

  record_findings <- report$findings |>
    dplyr::filter(.data$scope %in% c("record", "record_field"))
  expect_false(any(is.na(record_findings$record_id) | record_findings$record_id == ""))
})

test_that("build_quality_report supports raw CSV export paths", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    required_value = c("present", "")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  data_path <- tempfile(fileext = ".csv")
  metadata_path <- tempfile(fileext = ".csv")
  utils::write.csv(redcap_data, data_path, row.names = FALSE)
  utils::write.csv(metadata, metadata_path, row.names = FALSE)

  report <- build_quality_report(data = data_path, metadata = metadata_path)

  expect_equal(report$summaries$project$raw_row_count, 2)
  expect_false("supertbl_row_count" %in% names(report$summaries$project))
  expect_true("required_field_missing" %in% report$findings$issue)
})

test_that("raw reports drop descriptive text fields before analysis", {
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
    report <- build_quality_report(
      data = redcap_data,
      metadata = metadata,
      progress = "none"
    ),
    NA
  )

  expect_false("instructions" %in% report$metadata$fields$field_name)
  expect_false("instructions" %in% report$summaries$fields$field_name)
  expect_equal(report$summaries$project$field_count, 2)
  expect_equal(report$summaries$forms$field_count, 2)
  expect_false(any(report$findings$field_name == "instructions", na.rm = TRUE))
})

test_that("raw field summaries ignore structural missingness from repeating instruments", {
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

  report <- build_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "missingness",
    progress = "none"
  )

  expect_equal(report$summaries$project$mean_missing_rate, 0)
  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name %in% c("main_value", "repeat_value")) |>
      dplyr::pull(.data$missing_rate),
    c(0, 0)
  )
  expect_equal(report$summaries$records$missing_field_count, c(0, 0, 0))
})

test_that("raw operational checks ignore structural completion blanks from repeating instruments", {
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

  report <- build_quality_report(
    data = redcap_data,
    metadata = metadata,
    checks = "operational",
    progress = "none"
  )

  expect_equal(nrow(report$findings), 0)
})

test_that("build_quality_report supports explicit quiet progress mode", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    required_value = c("present", "")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  report <- build_quality_report(
    data = redcap_data,
    metadata = metadata,
    progress = "none"
  )

  expect_s3_class(report, "redcap_quality_report")
  expect_named(report, c("findings", "summaries", "metadata"))
  expect_equal(report$summaries$project$raw_row_count, 2)
  expect_false("supertbl_row_count" %in% names(report$summaries$project))
})

test_that("build_quality_report progress display does not affect report generation", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    required_value = c("present", "")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  expect_no_error(
    utils::capture.output(
      utils::capture.output(
        report <- build_quality_report(
          data = redcap_data,
          metadata = metadata,
          progress = "show"
        ),
        type = "message"
      )
    )
  )
  expect_s3_class(report, "redcap_quality_report")
})

test_that("build_quality_report supports REDCapTidieR-style supertibbles", {
  redcap_data <- tibble::tibble(
    record_id = c("1", "2"),
    required_value = c("present", "")
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "required_value"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Required Value"),
    required_field = c("y", "y")
  )

  supertbl <- tibble::tibble(
    redcap_form_name = "main",
    redcap_form_label = "Main",
    redcap_data = list(redcap_data)
  )

  report <- build_quality_report(data = supertbl, metadata = metadata)

  expect_equal(report$metadata$source, "supertbl")
  expect_false("raw_row_count" %in% names(report$summaries$project))
  expect_equal(report$summaries$project$supertbl_row_count, 2)
  expect_true("required_field_missing" %in% report$findings$issue)
})

test_that("build_quality_report preserves REDCapTidieR form and event context", {
  supertbl <- tibble::tibble(
    redcap_form_name = c("demographics", "labs"),
    redcap_form_label = c("Demographics", "Labs"),
    structure = c("nonrepeating", "repeating"),
    redcap_data = list(
      tibble::tibble(
        record_id = c("1", "2"),
        redcap_event = c("baseline", "month_1"),
        age = c(40, NA),
        form_status_complete = c("Complete", "Incomplete")
      ),
      tibble::tibble(
        record_id = c("1", "2"),
        redcap_event = c("baseline", "month_1"),
        redcap_repeat_instance = c(1, 1),
        hemoglobin = c(12, NA),
        form_status_complete = c("Complete", "Incomplete")
      )
    ),
    redcap_metadata = list(
      tibble::tibble(
        field_name = c("record_id", "age"),
        field_label = c("Record ID", "Age"),
        field_type = c("text", "text"),
        required_field = c("", "y")
      ),
      tibble::tibble(
        field_name = c("record_id", "hemoglobin"),
        field_label = c("Record ID", "Hemoglobin"),
        field_type = c("text", "text"),
        required_field = c("", "y")
      )
    ),
    redcap_events = list(
      tibble::tibble(
        redcap_event = c("baseline", "month_1"),
        redcap_arm = c(1, 1),
        arm_name = c("Arm 1", "Arm 1")
      ),
      tibble::tibble(
        redcap_event = c("baseline", "month_1"),
        redcap_arm = c(1, 1),
        arm_name = c("Arm 1", "Arm 1")
      )
    )
  )

  report <- build_quality_report(supertbl)

  expect_false("unknown" %in% report$findings$form_name)
  expect_false("form_status" %in% report$findings$form_name)
  expect_true(all(stats::na.omit(report$findings$form_name) %in% c("demographics", "labs")))
  expect_true(any(report$findings$event_name == "month_1", na.rm = TRUE))
  expect_equal(sort(unique(report$summaries$events$event_name)), c("baseline", "month_1"))
  expect_false("raw_row_count" %in% names(report$summaries$project))
  expect_equal(report$summaries$project$supertbl_row_count, 4)
  expect_equal(report$summaries$project$field_count, 3)
  expect_equal(report$summaries$project$form_count, 2)
  expect_equal(report$summaries$project$event_count, 2)
  expect_true(report$summaries$project$repeating_enabled)
  expect_equal(
    report$metadata$instrument_structure$structure,
    c("nonrepeating", "repeating")
  )
  record_findings <- report$findings |>
    dplyr::filter(.data$scope %in% c("record", "record_field"))
  expect_false(any(is.na(record_findings$record_id) | record_findings$record_id == ""))
})

test_that("supertbl field summaries ignore structural missingness from other forms", {
  supertbl <- tibble::tibble(
    redcap_form_name = c("demographics", "labs"),
    redcap_form_label = c("Demographics", "Labs"),
    redcap_data = list(
      tibble::tibble(
        record_id = c("1", "2"),
        age = c(40, 50),
        form_status_complete = c("Complete", "Complete")
      ),
      tibble::tibble(
        record_id = c("1", "2"),
        hemoglobin = c(12, 13),
        form_status_complete = c("Complete", "Complete")
      )
    ),
    redcap_metadata = list(
      tibble::tibble(
        field_name = c("record_id", "age"),
        field_label = c("Record ID", "Age"),
        field_type = c("text", "text"),
        required_field = c("", "y")
      ),
      tibble::tibble(
        field_name = c("record_id", "hemoglobin"),
        field_label = c("Record ID", "Hemoglobin"),
        field_type = c("text", "text"),
        required_field = c("", "y")
      )
    )
  )

  report <- build_quality_report(supertbl, checks = "missingness", progress = "none")

  expect_equal(report$summaries$project$mean_missing_rate, 0)
  expect_equal(nrow(report$findings), 0)
  expect_equal(
    report$summaries$fields |>
      dplyr::filter(.data$field_name %in% c("age", "hemoglobin")) |>
      dplyr::pull(.data$missing_rate),
    c(0, 0)
  )
})

test_that("raw and supertbl checkbox consistency findings align", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("main", "main"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    select_choices_or_calculations = c(NA, "none, None | fever, Fever")
  )

  raw_report <- build_quality_report(
    data = tibble::tibble(
      record_id = c("1", "2"),
      symptoms___none = c(1, 0),
      symptoms___fever = c(1, 1)
    ),
    metadata = metadata,
    checks = "consistency",
    progress = "none"
  )

  supertbl_report <- build_quality_report(
    data = tibble::tibble(
      redcap_form_name = "main",
      redcap_form_label = "Main",
      redcap_data = list(tibble::tibble(
        record_id = c("1", "2"),
        symptoms___none_raw = c(1, 0),
        symptoms___fever_raw = c(1, 1)
      )),
      redcap_metadata = list(metadata)
    ),
    checks = "consistency",
    progress = "none"
  )

  raw_findings <- raw_report$findings |>
    dplyr::select("record_id", "form_name", "event_name", "field_name", "issue")
  supertbl_findings <- supertbl_report$findings |>
    dplyr::select("record_id", "form_name", "event_name", "field_name", "issue")

  expect_equal(raw_findings, supertbl_findings)
  expect_equal(raw_report$findings$value, supertbl_report$findings$value)
})

test_that("supertbl checkbox label-only columns are treated as checked when populated", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("main", "main"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    select_choices_or_calculations = c(NA, "none, None | fever, Fever")
  )

  report <- build_quality_report(
    data = tibble::tibble(
      redcap_form_name = "main",
      redcap_form_label = "Main",
      redcap_data = list(tibble::tibble(
        record_id = c("1", "2"),
        symptoms___none_label = c("None", NA),
        symptoms___fever_label = c("Fever", "Fever")
      )),
      redcap_metadata = list(metadata)
    ),
    checks = "consistency",
    progress = "none"
  )

  expect_equal(report$findings$record_id, "1")
  expect_equal(report$findings$field_name, "symptoms")
})

test_that("supertbl checkbox raw columns are preferred over canonical columns", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "symptoms"),
    form_name = c("main", "main"),
    field_type = c("text", "checkbox"),
    field_label = c("Record ID", "Symptoms"),
    select_choices_or_calculations = c(NA, "none, None | fever, Fever")
  )

  report <- build_quality_report(
    data = tibble::tibble(
      redcap_form_name = "main",
      redcap_form_label = "Main",
      redcap_data = list(tibble::tibble(
        record_id = "1",
        symptoms___none = 0,
        symptoms___none_raw = 1,
        symptoms___none_label = "None",
        symptoms___fever = 1,
        symptoms___fever_raw = 1,
        symptoms___fever_label = "Fever"
      )),
      redcap_metadata = list(metadata)
    ),
    checks = "consistency",
    progress = "none"
  )

  expect_equal(report$findings$record_id, "1")
  expect_equal(report$findings$value, "symptoms___none")
})

test_that("supertbl checkbox normalization is a no-op without checkbox columns", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "name"),
    form_name = c("main", "main"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Name")
  )

  report <- build_quality_report(
    data = tibble::tibble(
      redcap_form_name = "main",
      redcap_form_label = "Main",
      redcap_data = list(tibble::tibble(
        record_id = c("1", "2"),
        name = c("one", "two")
      )),
      redcap_metadata = list(metadata)
    ),
    checks = "consistency",
    progress = "none"
  )

  expect_equal(nrow(report$findings), 0)
  expect_equal(report$summaries$project$record_count, 2)
})

test_that("supertbl checkbox metadata is canonicalized before duplicate label checks", {
  raw_metadata <- tibble::tibble(
    field_name = c("record_id", "survey_checkbox", "repeatsurvey_checkbox"),
    form_name = c("survey", "survey", "repeat_survey"),
    field_type = c("text", "checkbox", "checkbox"),
    field_label = c("Record ID", "Checkbox Field:", "Checkbox Field:"),
    select_choices_or_calculations = c(
      NA,
      "one, Choice 1 | two, Choice 2 | three, Choice 3",
      "one, Choice 1 | two, Choice 2 | three, Choice 3"
    )
  )

  raw_report <- build_quality_report(
    data = tibble::tibble(
      record_id = "1",
      survey_checkbox___one = 1,
      survey_checkbox___two = 0,
      repeatsurvey_checkbox___one = 1,
      repeatsurvey_checkbox___two = 0
    ),
    metadata = raw_metadata,
    checks = "metadata",
    progress = "none"
  )

  supertbl_metadata <- list(
    tibble::tibble(
      field_name = c(
        "record_id",
        "survey_checkbox___one",
        "survey_checkbox___two",
        "survey_checkbox___three"
      ),
      field_label = c(
        "Record ID",
        "Checkbox Field: Choice 1",
        "Checkbox Field: Choice 2",
        "Checkbox Field: Choice 3"
      ),
      field_type = c("text", "checkbox", "checkbox", "checkbox")
    ),
    tibble::tibble(
      field_name = c(
        "record_id",
        "repeatsurvey_checkbox___one",
        "repeatsurvey_checkbox___two",
        "repeatsurvey_checkbox___three"
      ),
      field_label = c(
        "Record ID",
        "Checkbox Field: Choice 1",
        "Checkbox Field: Choice 2",
        "Checkbox Field: Choice 3"
      ),
      field_type = c("text", "checkbox", "checkbox", "checkbox")
    )
  )

  supertbl_report <- build_quality_report(
    data = tibble::tibble(
      redcap_form_name = c("survey", "repeat_survey"),
      redcap_form_label = c("Survey", "Repeat Survey"),
      redcap_data = list(
        tibble::tibble(
          record_id = "1",
          survey_checkbox___one_raw = 1,
          survey_checkbox___two_raw = 0
        ),
        tibble::tibble(
          record_id = "1",
          repeatsurvey_checkbox___one_raw = 1,
          repeatsurvey_checkbox___two_raw = 0
        )
      ),
      redcap_metadata = supertbl_metadata
    ),
    checks = "metadata",
    progress = "none"
  )

  raw_duplicate <- raw_report$findings |>
    dplyr::filter(.data$issue == "duplicate_field_label") |>
    dplyr::select("field_name", "value")
  supertbl_duplicate <- supertbl_report$findings |>
    dplyr::filter(.data$issue == "duplicate_field_label") |>
    dplyr::select("field_name", "value")

  expect_equal(raw_duplicate, supertbl_duplicate)
  expect_false(any(grepl("___", supertbl_duplicate$field_name)))
})

test_that("supertbl metadata is deduplicated before field analytics", {
  supertbl <- tibble::tibble(
    redcap_form_name = c("demographics", "labs"),
    redcap_form_label = c("Demographics", "Labs"),
    redcap_data = list(
      tibble::tibble(
        record_id = c("1", "2"),
        age = c(40, NA),
        form_status_complete = c("Complete", "Complete")
      ),
      tibble::tibble(
        record_id = c("1", "2"),
        hemoglobin = c(12, 13),
        form_status_complete = c("Complete", "Complete")
      )
    ),
    redcap_metadata = list(
      tibble::tibble(
        field_name = c("record_id", "age"),
        field_label = c("Record ID", "Age"),
        field_type = c("text", "text"),
        required_field = c("", "")
      ),
      tibble::tibble(
        field_name = c("record_id", "hemoglobin"),
        field_label = c("Record ID", "Hemoglobin"),
        field_type = c("text", "text"),
        required_field = c("", "")
      )
    )
  )

  report <- build_quality_report(supertbl, checks = "missingness", progress = "none")

  expect_equal(sum(report$metadata$fields$field_name == "record_id"), 1)
  expect_equal(sum(report$summaries$fields$field_name == "record_id"), 1)
  expect_equal(report$summaries$project$field_count, 3)
  expect_equal(report$summaries$project$mean_missing_rate, 1 / 6)
  expect_equal(
    report$summaries$forms |>
      dplyr::arrange(.data$form_name) |>
      dplyr::select("form_name", "field_count"),
    tibble::tibble(
      form_name = c("demographics", "labs"),
      field_count = c(2L, 1L)
    )
  )
})

test_that("raw and supertbl nonrepeating form status findings align", {
  raw_data <- tibble::tibble(
    record_id = c("1", "2"),
    name = c("one", "two"),
    lab_value = c("ok", NA),
    demographics_complete = c(2, 2),
    labs_complete = c(2, NA)
  )

  metadata <- tibble::tibble(
    field_name = c("record_id", "name", "lab_value"),
    form_name = c("demographics", "demographics", "labs"),
    field_type = c("text", "text", "text"),
    field_label = c("Record ID", "Name", "Lab Value"),
    required_field = c("", "", "")
  )

  supertbl <- tibble::tibble(
    redcap_form_name = c("demographics", "labs"),
    redcap_form_label = c("Demographics", "Labs"),
    structure = c("nonrepeating", "nonrepeating"),
    redcap_data = list(
      tibble::tibble(
        record_id = c("1", "2"),
        name = c("one", "two"),
        form_status_complete = c("Complete", "Complete")
      ),
      tibble::tibble(
        record_id = "1",
        lab_value = "ok",
        form_status_complete = "Complete"
      )
    ),
    redcap_metadata = list(
      metadata |>
        dplyr::filter(.data$form_name == "demographics"),
      metadata |>
        dplyr::filter(.data$form_name == "labs")
    )
  )

  raw_report <- build_quality_report(
    data = raw_data,
    metadata = metadata,
    checks = "operational",
    progress = "none"
  )
  supertbl_report <- build_quality_report(
    data = supertbl,
    checks = "operational",
    progress = "none"
  )

  raw_findings <- raw_report$findings |>
    dplyr::select("record_id", "form_name", "event_name", "field_name")
  supertbl_findings <- supertbl_report$findings |>
    dplyr::select("record_id", "form_name", "event_name", "field_name")

  expect_equal(raw_findings, supertbl_findings)
  expect_equal(nrow(supertbl_findings), 1)
  expect_equal(supertbl_findings$field_name, "labs_complete")
})

test_that("supertbl form status findings use canonical REDCap field names", {
  supertbl <- tibble::tibble(
    redcap_form_name = c("demographics", "labs"),
    redcap_form_label = c("Demographics", "Labs"),
    structure = c("nonrepeating", "nonrepeating"),
    redcap_data = list(
      tibble::tibble(
        record_id = "1",
        name = "one",
        form_status_complete = "Incomplete"
      ),
      tibble::tibble(
        record_id = "1",
        lab_value = "ok",
        form_status_complete = "Unverified"
      )
    ),
    redcap_metadata = list(
      tibble::tibble(
        field_name = c("record_id", "name"),
        form_name = c("demographics", "demographics"),
        field_type = c("text", "text"),
        field_label = c("Record ID", "Name")
      ),
      tibble::tibble(
        field_name = c("record_id", "lab_value"),
        form_name = c("labs", "labs"),
        field_type = c("text", "text"),
        field_label = c("Record ID", "Lab Value")
      )
    )
  )

  report <- build_quality_report(
    data = supertbl,
    checks = "operational",
    progress = "none"
  )

  expect_equal(
    sort(report$findings$field_name),
    c("demographics_complete", "labs_complete")
  )
  expect_false("form_status_complete" %in% report$findings$field_name)
})

test_that("raw and supertbl completion status values are normalized", {
  metadata <- tibble::tibble(
    field_name = c("record_id", "name"),
    form_name = c("demographics", "demographics"),
    field_type = c("text", "text"),
    field_label = c("Record ID", "Name")
  )

  raw_report <- build_quality_report(
    data = tibble::tibble(
      record_id = c("1", "2"),
      name = c("one", "two"),
      demographics_complete = c(0, 1)
    ),
    metadata = metadata,
    checks = "operational",
    progress = "none"
  )

  supertbl_report <- build_quality_report(
    data = tibble::tibble(
      redcap_form_name = "demographics",
      redcap_form_label = "Demographics",
      redcap_data = list(tibble::tibble(
        record_id = c("1", "2"),
        name = c("one", "two"),
        form_status_complete = c("Incomplete", "Unverified")
      )),
      redcap_metadata = list(metadata)
    ),
    checks = "operational",
    progress = "none"
  )

  expect_equal(raw_report$findings$value, c("Incomplete", "Unverified"))
  expect_equal(raw_report$findings$value, supertbl_report$findings$value)
  expect_equal(raw_report$findings$expected, c("Complete", "Complete"))
})

test_that("supertbl completion status does not synthesize repeating instances", {
  supertbl <- tibble::tibble(
    redcap_form_name = c("demographics", "powers"),
    redcap_form_label = c("Demographics", "Powers"),
    structure = c("nonrepeating", "repeating"),
    redcap_data = list(
      tibble::tibble(
        record_id = c("1", "2"),
        name = c("one", "two"),
        form_status_complete = c("Complete", "Complete")
      ),
      tibble::tibble(
        record_id = "1",
        redcap_repeat_instance = 1,
        power = "flight",
        form_status_complete = "Complete"
      )
    ),
    redcap_metadata = list(
      tibble::tibble(
        field_name = c("record_id", "name"),
        form_name = c("demographics", "demographics"),
        field_type = c("text", "text"),
        field_label = c("Record ID", "Name")
      ),
      tibble::tibble(
        field_name = c("record_id", "power"),
        form_name = c("powers", "powers"),
        field_type = c("text", "text"),
        field_label = c("Record ID", "Power")
      )
    )
  )

  report <- build_quality_report(
    data = supertbl,
    checks = "operational",
    progress = "none"
  )

  expect_equal(nrow(report$findings), 0)
})

test_that("supertbl row count reports bound instrument rows only", {
  supertbl <- tibble::tibble(
    redcap_form_name = c("vitals", "labs"),
    redcap_form_label = c("Vitals", "Labs"),
    structure = c("repeating", "repeating"),
    redcap_data = list(
      tibble::tibble(
        record_id = c("1", "1"),
        redcap_event = c("visit", "visit"),
        redcap_arm = c(1, 1),
        redcap_repeat_instance = c(1, 2),
        weight = c(70, 72),
        form_status_complete = c("Complete", "Complete")
      ),
      tibble::tibble(
        record_id = c("1", "1"),
        redcap_event = c("visit", "visit"),
        redcap_arm = c(1, 1),
        redcap_repeat_instance = c(1, 2),
        hemoglobin = c(12, 13),
        form_status_complete = c("Complete", "Complete")
      )
    ),
    redcap_metadata = list(
      tibble::tibble(
        field_name = c("record_id", "weight"),
        field_label = c("Record ID", "Weight"),
        field_type = c("text", "text")
      ),
      tibble::tibble(
        field_name = c("record_id", "hemoglobin"),
        field_label = c("Record ID", "Hemoglobin"),
        field_type = c("text", "text")
      )
    ),
    redcap_events = list(
      tibble::tibble(
        redcap_event = "visit",
        redcap_arm = 1,
        arm_name = "Arm 1",
        repeat_type = "repeat_together"
      ),
      tibble::tibble(
        redcap_event = "visit",
        redcap_arm = 1,
        arm_name = "Arm 1",
        repeat_type = "repeat_together"
      )
    )
  )

  report <- build_quality_report(supertbl)

  expect_false("raw_row_count" %in% names(report$summaries$project))
  expect_equal(report$summaries$project$supertbl_row_count, 4)
})
