test_that("quality report viewer functions validate inputs", {
  expect_error(
    view_quality_report(tibble::tibble()),
    "redcap_quality_report"
  )
  expect_error(
    view_quality_report(mock_quality_report, page_length = 0),
    "page_length"
  )
  expect_error(
    view_quality_report(mock_quality_report, page_length = 1.5),
    "page_length"
  )

  output_file <- tempfile(fileext = ".html")
  expect_error(
    save_quality_report(tibble::tibble(), output_file),
    "redcap_quality_report"
  )
  expect_error(save_quality_report(mock_quality_report, ""), "file")
})

test_that("view_quality_report returns a browsable report viewer", {
  report <- mock_quality_report
  report$metadata$record_id_field <- "record_id"
  report$metadata$source <- "api"

  viewer <- view_quality_report(report, page_length = 5)
  output <- as.character(viewer)

  expect_s3_class(viewer, "shiny.tag.list")
  expect_true(attr(viewer, "browsable_html"))
  expect_match(output, "REDCap Quality Report")
  expect_match(output, "Interactive findings review")
  expect_match(output, "data-tab-group=\"primary\"")
  expect_match(output, "data-section=\"overview\"")
  expect_match(output, "data-section=\"findings\"")
  expect_match(output, "data-section=\"summaries\"")
  expect_match(output, "data-section=\"metadata\"")
  expect_match(output, "Record ID field")
  expect_match(output, "record_id")
  expect_match(output, "Source")
  expect_match(output, "api")
  expect_match(output, "Missing rate")
  expect_match(output, "%")

  dependencies <- htmltools::renderTags(viewer)$dependencies
  dependency_names <- vapply(dependencies, `[[`, character(1), "name")
  expect_true("redcap-quality-report-viewer" %in% dependency_names)
})

test_that("quality report viewer includes report tables and descriptions", {
  viewer <- view_quality_report(mock_quality_report)
  output <- as.character(viewer)

  expect_match(output, "data-section=\"summary-forms\"")
  expect_match(output, "data-section=\"summary-fields\"")
  expect_match(output, "data-section=\"summary-records\"")
  expect_match(output, "data-section=\"summary-events\"")
  expect_match(output, "data-section=\"metadata-fields\"")
  expect_match(output, "data-section=\"metadata-event-instruments\"")
  expect_match(output, "data-section=\"metadata-repeating\"")
  expect_match(output, "weighted fraction missing")
  expect_match(output, "One row per detected issue")
  expect_match(output, "One row per field with branching logic")
  expect_match(output, "finding_id")
  expect_match(output, "high_risk_free_text")
})

test_that("quality report viewer uses select filters for categorical columns", {
  findings <- get_quality_report_viewer_data(mock_quality_report$findings)
  fields <- get_quality_report_viewer_data(mock_quality_report$metadata$fields)

  expect_s3_class(findings$check, "factor")
  expect_s3_class(findings$issue, "factor")
  expect_s3_class(findings$severity, "factor")
  expect_s3_class(findings$scope, "factor")
  expect_s3_class(findings$form_name, "factor")
  expect_type(findings$record_id, "character")
  expect_type(findings$value, "character")
  expect_type(findings$message, "character")

  expect_s3_class(fields$form_name, "factor")
  expect_s3_class(fields$field_type, "factor")
  expect_s3_class(fields$required_field, "factor")
  expect_type(fields$field_name, "character")
  expect_type(fields$field_label, "character")

  output <- as.character(view_quality_report(mock_quality_report))
  expect_true(grepl("data-type=\\\"factor\\\"", output, fixed = TRUE))
  expect_match(output, "<select")
})

test_that("quality report viewer handles empty and optional sections", {
  report <- mock_quality_report
  report$findings <- report$findings[0, ]
  report$summaries$project$finding_count <- 0L
  report$summaries$events <- tibble::tibble()
  report$metadata$events <- tibble::tibble()
  report$metadata$event_instruments <- tibble::tibble()
  report$metadata$repeating_instruments <- tibble::tibble()

  viewer <- view_quality_report(report)
  output <- as.character(viewer)

  expect_match(output, "No findings were generated")
  expect_false(grepl(
    "data-section=\"summary-events\"",
    output,
    fixed = TRUE
  ))
  expect_false(grepl(
    "data-section=\"metadata-events\"",
    output,
    fixed = TRUE
  ))
  expect_false(grepl(
    "data-section=\"metadata-event-instruments\"",
    output,
    fixed = TRUE
  ))
  expect_false(grepl(
    "data-section=\"metadata-repeating\"",
    output,
    fixed = TRUE
  ))
  expect_match(output, "data-section=\"metadata-validation\"")
  expect_match(output, "data-section=\"metadata-branching\"")
})

test_that("quality report viewer escapes REDCap-provided text", {
  report <- mock_quality_report
  report$metadata$fields$field_label[[1]] <- "<script>alert('x')</script>"

  viewer <- view_quality_report(report)
  output <- as.character(viewer)

  expect_match(output, "&lt;script&gt;alert")
  expect_false(grepl("<script>alert", output, fixed = TRUE))
})

test_that("quality report viewer assets match snapshots", {
  asset_dir <- system.file("quality-report-viewer", package = "REDCapExploreR")

  expect_snapshot_file(
    file.path(asset_dir, "quality-report.css"),
    name = "quality-report.css",
    cran = TRUE
  )
  expect_snapshot_file(
    file.path(asset_dir, "quality-report.js"),
    name = "quality-report.js",
    cran = TRUE
  )
})

test_that("save_quality_report writes one HTML file with inlined dependencies", {
  output_dir <- tempdir()
  output_file <- file.path(output_dir, "saved-quality-report.html")

  result <- save_quality_report(mock_quality_report, output_file)

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))
  expect_false(dir.exists(file.path(output_dir, "saved-quality-report_files")))
  output <- paste(readLines(output_file, warn = FALSE), collapse = "\n")
  expect_match(output, "REDCap Quality Report")
  expect_match(output, "redcap-quality-report-viewer")
  expect_match(output, "data-redcap-quality-report-source")
  expect_false(grepl("saved-quality-report_files", output, fixed = TRUE))
})
