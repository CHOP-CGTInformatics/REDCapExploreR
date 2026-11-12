test_that("mock record status data reflects current record-status logic", {
  project <- REDCapExploreR:::get_quality_project_data(mock_redcap_project)

  expect_equal(
    REDCapExploreR:::get_record_status_tiles(project),
    mock_record_status_data
  )
})

test_that("mock codebook is consistent with its extended metadata", {
  project <- mock_redcap_project
  project$metadata <- mock_codebook$definitions$fields
  project$project_info <- mock_codebook$definitions$project
  rebuilt <- get_codebook(get_codebook_project(project))
  tables <- setdiff(names(mock_codebook), "definitions")
  expect_equal(rebuilt[tables], mock_codebook[tables])
})

test_that("mock relationship example includes inputs, indirect uses, and interpretation notes", {
  report <- field_relationship_summary("weight_kg", mock_codebook, include_diagnostics = TRUE)
  expect_equal(report$upstream$item_name, "baseline_weight_kg")
  review <- dplyr::filter(report$downstream, .data$item_name == "weight_review")
  expect_equal(review$depth, 2L)
  expect_equal(review$field_path[[1]], c("weight_kg", "bmi", "weight_review"))
  expect_setequal(unique(report$references$relationship), c(
    "initial_value", "visibility_condition", "calculation_input", "displayed_value",
    "embedded_field", "annotation_condition", "unclassified"
  ))
  expect_setequal(unique(report$references$resolution), c("resolved", "context_dependent", "unclassified"))
  expect_true(all(nzchar(report$references$status_note[report$references$resolution != "resolved"])))
  missing <- dplyr::filter(report$diagnostics, .data$row_kind == "reference", .data$status == "unresolved")
  expect_match(missing$expression, "retired_weight", fixed = TRUE)
  expect_lt(nrow(report$coverage), 20L)
})

test_that("mock quality report reflects current quality-report logic", {
  project <- REDCapExploreR:::get_quality_project_data(mock_redcap_project)

  expect_equal(
    REDCapExploreR:::get_quality_report(
      project = project,
      checks = c(
        "missingness",
        "metadata",
        "outliers",
        "operational",
        "consistency"
      ),
      sparse_threshold = 0.95,
      outlier_iqr_multiplier = 3,
      evaluation_date = mock_quality_report$metadata$settings$evaluation_date
    ),
    mock_quality_report
  )
})
