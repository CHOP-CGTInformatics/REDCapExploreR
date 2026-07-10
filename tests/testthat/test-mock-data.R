test_that("mock record status data reflects current record-status logic", {
  project <- REDCapExploreR:::get_quality_project_data(mock_redcap_project)

  expect_equal(
    REDCapExploreR:::get_record_status_tiles(project),
    mock_record_status_data
  )
})

test_that("mock codebook reflects current codebook logic", {
  project <- REDCapExploreR:::get_codebook_project(mock_redcap_project)

  expect_equal(
    REDCapExploreR:::get_codebook(project),
    mock_codebook
  )
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
      outlier_iqr_multiplier = 3
    ),
    mock_quality_report
  )
})
