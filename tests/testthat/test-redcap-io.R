test_that("API permission failures are rejected for record and structural endpoints", {
  failed <- list(data = tibble::tibble(), success = FALSE, status_code = 403L, raw_text = "Permission denied")
  expect_error(get_redcapr_data(failed, "records"), "unsuccessful")
  expect_error(get_redcapr_data(failed, "event-instrument metadata"), "unsuccessful")
})

test_that("successful API envelopes distinguish empty tables from malformed responses", {
  expect_equal(get_redcapr_data(list(data = tibble::tibble(), success = TRUE), "records"), tibble::tibble())
  expect_equal(get_redcapr_data(tibble::tibble(x = 1), "records"), tibble::tibble(x = 1))
  expect_error(get_redcapr_data(list(data = NULL, success = TRUE), "records"), "data frame")
})

test_that("classic event responses are accepted only by the event endpoint", {
  response <- list(
    data = tibble::tibble(), success = FALSE,
    raw_text = "ERROR: You cannot export events for classic projects"
  )
  expect_equal(get_redcapr_data(response, "event metadata"), tibble::tibble())
  expect_error(get_redcapr_data(response, "records"), "unsuccessful")
  expect_error(get_redcapr_data(response, "event-instrument metadata"), "unsuccessful")
})

test_that("classic form-event mapping responses require an exact endpoint-specific message", {
  response <- list(
    data = tibble::tibble(), success = FALSE,
    raw_text = "ERROR: You cannot export form/event mappings for classic projects"
  )
  expect_equal(get_redcapr_data(response, "event-instrument metadata"), tibble::tibble())
  expect_error(get_redcapr_data(response, "metadata"), "unsuccessful")
  response$raw_text <- paste(response$raw_text, "Permission denied")
  expect_error(get_redcapr_data(response, "event-instrument metadata"), "unsuccessful")
})

test_that("classic arm responses do not excuse failures from other endpoints", {
  response <- list(
    data = tibble::tibble(), success = FALSE,
    raw_text = "ERROR: You cannot export arms for classic projects"
  )
  expect_equal(get_redcapr_data(response, "arm metadata"), tibble::tibble())
  expect_error(get_redcapr_data(response, "records"), "unsuccessful")
})

test_that("projects without repeats accept the documented repeating-metadata response", {
  response <- list(
    data = tibble::tibble(), success = FALSE,
    raw_text = paste(
      "ERROR: You cannot export repeating instruments and events because",
      "the project does not contain any repeating instruments and events"
    )
  )
  expect_equal(get_redcapr_data(response, "repeating-instrument metadata"), tibble::tibble())
  expect_error(get_redcapr_data(response, "records"), "unsuccessful")
})

test_that("classic projects without repeats build all public reports", {
  failed <- function(text) list(data = tibble::tibble(), success = FALSE, status_code = 400L, raw_text = text)
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(...) {
      list(
        data = tibble::tibble(record_id = "1", value = "a", demographics_complete = "2", main_complete = "2"), success = TRUE
      )
    },
    redcap_metadata_read = function(...) {
      list(
        data = get_test_metadata(c("record_id", "value"), c("demographics", "main")), success = TRUE
      )
    },
    redcap_event_read = function(...) failed("ERROR: You cannot export events for classic projects"),
    redcap_event_instruments = function(...) failed("ERROR: You cannot export form/event mappings for classic projects"),
    redcap_arm_export = function(...) failed("ERROR: You cannot export arms for classic projects"),
    redcap_instruments = function(...) {
      list(
        data = tibble::tibble(instrument_name = c("demographics", "main"), instrument_label = c("Demographics", "Main")), success = TRUE
      )
    },
    redcap_instrument_repeating = function(...) {
      failed(paste(
        "ERROR: You cannot export repeating instruments and events because",
        "the project does not contain any repeating instruments and events"
      ))
    },
    redcap_project_info_read = function(...) {
      list(
        data = tibble::tibble(is_longitudinal = FALSE, has_repeating_instruments_or_events = FALSE), success = TRUE
      )
    }
  )
  tiles <- build_record_status_data("https://example.invalid", "synthetic")
  expect_equal(tiles$pct_complete[tiles$form_name == "Main"], 1)
  book <- build_codebook("https://example.invalid", "synthetic")
  expect_equal(nrow(book$events), 0L)
  expect_equal(nrow(book$event_instruments), 0L)
  expect_equal(book$repeating$form_name, character())
  report <- build_quality_report("https://example.invalid", "synthetic", checks = "missingness", progress = "none")
  expect_equal(nrow(report$findings), 0L)
})

test_that("classic projects with repeats preserve instance completion and codebook configuration", {
  failed <- function(text) list(data = tibble::tibble(), success = FALSE, status_code = 400L, raw_text = text)
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(...) {
      list(
        data = tibble::tibble(
          record_id = rep("1", 3), value = c(NA, "a", "b"), main_complete = c(NA, "0", "2"),
          demographics_complete = c("2", NA, NA), redcap_repeat_instrument = c(NA, "main", "main"),
          redcap_repeat_instance = c(NA, "1", "2")
        ), success = TRUE
      )
    },
    redcap_metadata_read = function(...) {
      list(
        data = get_test_metadata(c("record_id", "value"), c("demographics", "main")), success = TRUE
      )
    },
    redcap_event_read = function(...) failed("ERROR: You cannot export events for classic projects"),
    redcap_event_instruments = function(...) failed("ERROR: You cannot export form/event mappings for classic projects"),
    redcap_arm_export = function(...) failed("ERROR: You cannot export arms for classic projects"),
    redcap_instruments = function(...) {
      list(
        data = tibble::tibble(instrument_name = c("demographics", "main"), instrument_label = c("Demographics", "Main")), success = TRUE
      )
    },
    redcap_instrument_repeating = function(...) {
      list(data = tibble::tibble(form_name = "main", custom_form_label = "[value]"), success = TRUE)
    },
    redcap_project_info_read = function(...) {
      list(
        data = tibble::tibble(is_longitudinal = FALSE, has_repeating_instruments_or_events = TRUE), success = TRUE
      )
    }
  )
  tiles <- build_record_status_data("https://example.invalid", "synthetic")
  expect_equal(tiles$pct_complete[tiles$form_name == "Main"], 0.5)
  book <- build_codebook("https://example.invalid", "synthetic")
  expect_equal(nrow(book$events), 0L)
  expect_equal(nrow(book$event_instruments), 0L)
  expect_equal(book$repeating$form_name, "main")
  report <- build_quality_report("https://example.invalid", "synthetic", checks = "missingness", progress = "none")
  expect_equal(nrow(report$findings), 0L)
})

test_that("public workflows reject unsuccessful structural API envelopes", {
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(...) list(data = tibble::tibble(record_id = "1"), success = TRUE),
    redcap_metadata_read = function(...) list(data = get_test_metadata("record_id"), success = TRUE),
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

test_that("gray status export is requested and remains distinct from incomplete", {
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(..., blank_for_gray_form_status) {
      expect_true(blank_for_gray_form_status)
      list(data = tibble::tibble(
        record_id = c("gray", "incomplete", "stored"),
        value = c(NA, NA, "x"), main_complete = c(NA, "0", NA)
      ))
    },
    redcap_metadata_read = function(...) list(data = get_test_metadata(c("record_id", "value"))),
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

test_that("endpoint diagnostics redact credentials and retain the cause", {
  error <- tryCatch(pull_redcap_table(
    function(...) stop("Access denied for synthetic-token at https://example.invalid"),
    "https://example.invalid", "synthetic-token", "metadata"
  ), error = function(cnd) cnd)
  expect_match(conditionMessage(error), "Access denied")
  expect_false(grepl("synthetic-token", conditionMessage(error), fixed = TRUE))
  expect_false(grepl("https://example.invalid", conditionMessage(error), fixed = TRUE))
})

test_that("pull_redcap_project requests quiet raw REDCapR records", {
  quiet_records <- function(
    redcap_uri,
    token,
    raw_or_label,
    raw_or_label_headers,
    export_checkbox_label,
    na,
    guess_type,
    verbose,
    ...
  ) {
    expect_equal(raw_or_label, "raw")
    expect_equal(raw_or_label_headers, "raw")
    expect_false(export_checkbox_label)
    expect_equal(na, "")
    expect_false(guess_type)
    if (verbose) {
      message("records should be quiet")
    }
    list(data = tibble::tibble(record_id = "1"))
  }
  quiet_metadata <- function(redcap_uri, token, verbose, ...) {
    if (verbose) {
      message("metadata should be quiet")
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
    c(
      "data",
      "metadata",
      "events",
      "event_instruments",
      "instruments",
      "repeating_instruments"
    )
  )
})

test_that("pull_redcap_project reports optional and required API failures", {
  testthat::local_mocked_bindings(
    redcap_read_oneshot = function(redcap_uri, token, verbose, ...) {
      list(data = tibble::tibble(record_id = "1"))
    },
    redcap_metadata_read = function(redcap_uri, token, verbose, ...) {
      list(
        data = tibble::tibble(
          field_name = "record_id",
          form_name = "main",
          field_type = "text",
          field_label = "Record ID"
        )
      )
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

  expect_error(
    pull_redcap_project(
      redcap_uri = "https://redcap.example/api/",
      token = "test-token"
    ),
    "event metadata"
  )

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
