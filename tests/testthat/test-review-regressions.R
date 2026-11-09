get_review_metadata <- function(fields, forms = "main", types = "text") {
  tibble::tibble(
    field_name = fields, form_name = forms, field_type = types,
    field_label = fields
  )
}

build_review_report <- function(data, metadata, checks = "missingness", ...) {
  project <- get_quality_project_data(c(list(data = data, metadata = metadata), list(...)))
  get_quality_report(project, checks, 0.95, 3, evaluation_date = as.Date("2026-11-09"))
}

test_that("API envelopes distinguish failures from classic nonapplicability", {
  failed <- list(
    data = tibble::tibble(), success = FALSE, status_code = 403L,
    raw_text = "Permission denied"
  )
  purrr::walk(c("records", "metadata", "event metadata", "repeating-instrument metadata"), \(label) {
    expect_error(get_redcapr_data(failed, label), "unsuccessful")
  })
  failed$raw_text <- "ERROR: You cannot export events for classic projects"
  expect_equal(get_redcapr_data(failed, "event metadata"), tibble::tibble())
  expect_error(get_redcapr_data(failed, "records"), "unsuccessful")
  expect_equal(get_redcapr_data(list(data = tibble::tibble(), success = TRUE), "records"), tibble::tibble())
  expect_equal(get_redcapr_data(tibble::tibble(x = 1), "records"), tibble::tibble(x = 1))
  expect_error(get_redcapr_data(list(data = NULL, success = TRUE), "records"), "data frame")
})

test_that("public workflows reject unsuccessful structural API envelopes", {
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(...) list(data = tibble::tibble(record_id = "1"), success = TRUE),
    redcap_metadata_read = function(...) list(data = get_review_metadata("record_id"), success = TRUE),
    redcap_event_read = function(...) list(data = tibble::tibble(), success = TRUE),
    redcap_event_instruments = function(...) list(data = tibble::tibble(), success = TRUE),
    redcap_instruments = function(...) list(data = tibble::tibble(), success = TRUE),
    redcap_instrument_repeating = function(...) {
      list(data = tibble::tibble(), success = FALSE, status_code = 403L)
    }
  )
  expect_error(
    build_quality_report("https://example.invalid", "synthetic", progress = "none"),
    "repeating-instrument metadata"
  )
  expect_error(
    build_record_status_data("https://example.invalid", "synthetic"),
    "repeating-instrument metadata"
  )
  expect_error(build_codebook("https://example.invalid", "synthetic"), "repeating-instrument metadata")
})

test_that("zero-column empty records retain stable output schemas", {
  metadata <- get_review_metadata(c("study_id", "value", "options"), types = c("text", "text", "checkbox"))
  metadata$select_choices_or_calculations <- c(NA, NA, "1, One | 2, Two")
  project <- get_quality_project_data(list(data = tibble::tibble(), metadata = metadata))
  expect_equal(project$record_id_field, "study_id")
  expect_named(project$data, c("study_id", "value", "options___1", "options___2", "main_complete"))
  report <- get_quality_report(project, "missingness", 0.95, 3)
  expect_equal(report$summaries$project$record_count, 0)
  expect_equal(nrow(report$findings), 0)
  tiles <- get_record_status_tiles(project)
  expect_named(tiles, c("study_id", "form_name", "pct_complete"))
  expect_equal(nrow(tiles), 0)
  expect_s3_class(ggplot2::ggplot_build(plot_record_status(tiles)), "ggplot_built")
  expect_error(get_quality_project_data(list(data = tibble::tibble(value = "x"), metadata = metadata)),
    "record ID field",
    class = "rlang_error"
  )
})

test_that("gray status export is requested and remains distinct from incomplete", {
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(..., blank_for_gray_form_status) {
      expect_true(blank_for_gray_form_status)
      list(data = tibble::tibble(
        record_id = c("gray", "incomplete", "stored"),
        value = c(NA, NA, "x"), main_complete = c(NA, "0", NA)
      ))
    },
    redcap_metadata_read = function(...) list(data = get_review_metadata(c("record_id", "value"))),
    redcap_event_read = function(...) list(data = tibble::tibble()),
    redcap_event_instruments = function(...) list(data = tibble::tibble()),
    redcap_instruments = function(...) list(data = tibble::tibble()),
    redcap_instrument_repeating = function(...) list(data = tibble::tibble())
  )
  project <- get_quality_project("https://example.invalid", "synthetic")
  expect_equal(get_record_status_tiles(project)$pct_complete, c(NA, 0, NA))
  expect_equal(
    get_quality_report(project, "operational", 0.95, 3)$findings$record_id,
    c("incomplete", "stored")
  )
})

test_that("mixed repeating configurations are resolved by event and instrument", {
  data <- tibble::tibble(
    record_id = rep("1", 4),
    redcap_event_name = c("baseline_arm_1", "followup_arm_1", "followup_arm_1", "event_arm_2"),
    redcap_repeat_instrument = c(NA, "assessment", "assessment", NA),
    redcap_repeat_instance = c(NA, "1", "2", "1"),
    value = c(NA, NA, "x", NA), assessment_complete = c("2", "2", "0", "2")
  )
  metadata <- get_review_metadata(c("record_id", "value"), "assessment")
  metadata$required_field <- c("y", "y")
  events <- tibble::tibble(
    unique_event_name = c("baseline_arm_1", "followup_arm_1", "event_arm_2"),
    event_name = c("Baseline", "Followup", "Repeating event")
  )
  mapping <- tibble::tibble(unique_event_name = events$unique_event_name, form = "assessment")
  repeating <- tibble::tibble(
    event_name = c("followup_arm_1", "event_arm_2"),
    form_name = c("assessment", NA)
  )
  project <- get_quality_project_data(list(
    data = data, metadata = metadata, events = events,
    event_instruments = mapping, repeating_instruments = repeating
  ))
  expect_equal(get_record_status_tiles(project)$pct_complete, c(1, 0.5, 1))
  report <- get_quality_report(project, "missingness", 0.95, 3)
  expect_equal(
    report$findings$event_name[report$findings$issue == "required_field_missing"],
    events$unique_event_name
  )
})

test_that("display label collisions retain distinct plotting positions", {
  data <- tibble::tibble(
    record_id = c("1", "1"), redcap_event_name = c("visit_arm_1", "visit_arm_2"),
    a = "x", b = "y", first_complete = c("2", "0"), second_complete = c("0", "2")
  )
  metadata <- get_review_metadata(c("record_id", "a", "b"), c("first", "first", "second"))
  project <- get_quality_project_data(list(
    data = data, metadata = metadata,
    events = tibble::tibble(unique_event_name = data$redcap_event_name, event_name = "Visit"),
    instruments = tibble::tibble(instrument_name = c("first", "second"), instrument_label = "Same")
  ))
  tiles <- get_record_status_tiles(project)
  expect_length(levels(tiles$form_name), 4)
  expect_length(unique(ggplot2::ggplot_build(plot_record_status(tiles))$data[[1]]$x), 4)
  expect_true(all(grepl("visit_arm_", levels(tiles$form_name), fixed = TRUE)))
  project$data <- dplyr::select(project$data, -"redcap_event_name")
  tiles <- get_record_status_tiles(project)
  expect_equal(levels(tiles$form_name), c("Same [first]", "Same [second]"))
})

test_that("branching respects numeric types, categorical codes and literal contents", {
  data <- tibble::tibble(
    record_id = as.character(1:4), age = c("2", "10", "20", "100"),
    limit = c("1", "20", "10", "200"), code = c("01", "1", "2", "3"),
    answer = c("and", "a=b", "x[y]", "O'Brien"),
    blank = c(NA, "", "yes", "no"), options___1 = c("1", "0", "0", "1"),
    detail = NA_character_, main_complete = "2"
  )
  metadata <- get_review_metadata(c("record_id", "age", "limit", "code", "answer", "blank", "options", "detail"),
    types = c("text", "text", "text", "radio", "text", "text", "checkbox", "text")
  )
  metadata$text_validation_type_or_show_slider_number <- c(NA, "integer", "integer", rep(NA, 5))
  metadata$select_choices_or_calculations <- c(NA, NA, NA, "01, Zero one | 1, One | 2, Two | 3, Three", NA, NA, "1, One", NA)
  metadata$required_field <- c(rep("n", 7), "y")
  metadata$branching_logic <- c(rep(NA, 7), "[age] > 5")
  project <- get_quality_project_data(list(data = data, metadata = metadata))
  findings <- get_quality_report(project, "missingness", 0.95, 3)$findings
  expect_equal(findings$record_id[findings$issue == "required_field_missing"], c("2", "3", "4"))
  cases <- list(
    "[age] > 5" = c(FALSE, TRUE, TRUE, TRUE),
    "[age] > [limit]" = c(TRUE, FALSE, TRUE, FALSE),
    "[age] >= '20'" = c(FALSE, FALSE, TRUE, TRUE),
    "[code] = 1" = c(FALSE, TRUE, FALSE, FALSE),
    "[code] = '01'" = c(TRUE, FALSE, FALSE, FALSE),
    "[answer] = 'and'" = c(TRUE, FALSE, FALSE, FALSE),
    "[answer] = 'a=b'" = c(FALSE, TRUE, FALSE, FALSE),
    "[answer] = 'x[y]'" = c(FALSE, FALSE, TRUE, FALSE),
    "[answer] = \"O'Brien\"" = c(FALSE, FALSE, FALSE, TRUE),
    "[blank] = ''" = c(TRUE, TRUE, FALSE, FALSE),
    "[blank] <> ''" = c(FALSE, FALSE, TRUE, TRUE),
    "[options(1)] = '1'" = c(TRUE, FALSE, FALSE, TRUE),
    "([age] > 5 and [age] < 100) or [code] = '01'" = c(TRUE, TRUE, TRUE, FALSE)
  )
  purrr::iwalk(cases, \(expected, logic) expect_equal(get_branching_applicable_rows(project, logic), expected, info = logic))
  purrr::walk(
    c("[missing] = 1", "[event_arm_1][age] > 5", "datediff([age], 'today', 'y') > 1", "[age] + 1 > 5"),
    \(logic) expect_equal(get_branching_applicable_rows(project, logic), rep(TRUE, 4))
  )
  project$data$age <- c("-10.5", "-2.5", ".5", "10.5")
  project$metadata$text_validation_type_or_show_slider_number[2] <- "number"
  expect_equal(get_branching_applicable_rows(project, "[age] > -2.5"), c(FALSE, FALSE, TRUE, TRUE))
})

test_that("IQR uses eligible finite values and keeps assessing the report", {
  data <- tibble::tibble(
    record_id = as.character(1:6), redcap_event_name = c(rep("enabled_arm_1", 5), "disabled_arm_1"),
    value = c("1", "2", "3", "4", "5", "999"), main_complete = "2"
  )
  metadata <- get_review_metadata(c("record_id", "value"))
  report <- build_review_report(data, metadata, "outliers",
    event_instruments = tibble::tibble(unique_event_name = "enabled_arm_1", form = "main")
  )
  expect_equal(nrow(report$findings), 0)
  data <- dplyr::select(data, -"redcap_event_name")
  data$value <- c("1", "2", "3", "4", "Inf", "-Inf")
  expect_warning(report <- build_review_report(data, metadata, "outliers"), "excluded from IQR")
  expect_equal(nrow(report$findings), 0)
  data$value <- c("1", rep("Inf", 5))
  expect_warning(build_review_report(data, metadata, "outliers"), "excluded from IQR")
  expect_true(is.na(get_parsed_validation_values(paste(rep("9", 400), collapse = ""), "number")))
})

test_that("checkbox meaning follows metadata labels rather than legacy codes", {
  data <- tibble::tibble(record_id = "1", support___no = "1", support___vent = "1", main_complete = "2")
  metadata <- get_review_metadata(c("record_id", "support"), types = c("text", "checkbox"))
  metadata$select_choices_or_calculations <- c(NA, "no, Nasal oxygen | vent, Ventilation")
  expect_equal(nrow(build_review_report(data, metadata, "consistency")$findings), 0)
  metadata$select_choices_or_calculations[2] <- "no, None | vent, Ventilation"
  expect_equal(build_review_report(data, metadata, "consistency")$findings$issue, "checkbox_none_with_other")
})

test_that("report presentation exposes settings, assessment coverage and denominators", {
  data <- tibble::tibble(record_id = "1", detail = NA_character_, main_complete = "0")
  metadata <- get_review_metadata(c("record_id", "detail"))
  report <- build_review_report(data, metadata)
  expect_output(print(report), "No values were eligible")
  expect_equal(report$metadata$settings$checks, "missingness")
  expect_equal(report$metadata$settings$evaluation_date, as.Date("2026-11-09"))
  data$main_complete <- "2"
  report <- build_review_report(data, metadata)
  expect_match(report$findings$message, "1 of 1 assessed values are missing \\(100%\\); threshold: 95%")
  expect_equal(report$metadata$settings$package_version, as.character(utils::packageVersion("REDCapExploreR")))
  purrr::walk(c(Inf, NaN, .Machine$integer.max + 1), \(value) {
    expect_error(view_codebook(mock_codebook, page_length = value), "positive integer")
  })
})

test_that("instrument aliases normalize consistently without dropping original columns", {
  purrr::walk(c("instrument_name", "form_name", "instrument", "form"), \(column) {
    input <- tibble::as_tibble(stats::setNames(list("main"), column))
    normalized <- get_standard_instruments(input)
    expect_equal(normalized$form_name, "main")
    expect_true(is.na(normalized$form_label))
    expect_true(column %in% names(normalized))
  })
})

test_that("endpoint diagnostics redact credentials and retain the cause", {
  error <- tryCatch(pull_redcap_table(
    function(...) stop("Access denied for synthetic-token at https://example.invalid"),
    "https://example.invalid", "synthetic-token", "metadata"
  ), error = function(cnd) cnd)
  expect_match(conditionMessage(error), "Access denied")
  expect_false(grepl("synthetic-token", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("https://example.invalid", conditionMessage(error), fixed = TRUE))
})

test_that("invalid numeric values do not become blank branching references", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = c("1", "2"), value = c("bad", NA_character_)),
    metadata = dplyr::mutate(get_review_metadata(c("record_id", "value")),
      text_validation_type_or_show_slider_number = c(NA, "integer")
    )
  ))
  expect_equal(get_branching_applicable_rows(project, "[value] = ''"), c(FALSE, TRUE))
})
