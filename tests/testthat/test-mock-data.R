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
