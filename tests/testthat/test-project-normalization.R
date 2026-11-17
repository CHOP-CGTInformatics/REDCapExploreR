test_that("zero-column empty records retain stable output schemas", {
  metadata <- get_test_metadata(c("study_id", "value", "options"), types = c("text", "text", "checkbox"))
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

test_that("instrument name aliases normalize without losing their source columns", {
  expect_equal(
    get_standard_instruments(tibble::tibble(instrument_name = "main")),
    tibble::tibble(instrument_name = "main", form_name = "main", form_label = NA_character_)
  )
  expect_equal(
    get_standard_instruments(tibble::tibble(form_name = "main")),
    tibble::tibble(form_name = "main", form_label = NA_character_)
  )
  expect_equal(
    get_standard_instruments(tibble::tibble(instrument = "main")),
    tibble::tibble(instrument = "main", form_name = "main", form_label = NA_character_)
  )
  expect_equal(
    get_standard_instruments(tibble::tibble(form = "main")),
    tibble::tibble(form = "main", form_name = "main", form_label = NA_character_)
  )
})

test_that("completion status normalization accepts raw codes and legacy labels", {
  expect_equal(
    get_completion_status_value(c("0", "1", "2", NA, "Incomplete", "Unverified", "Complete")),
    c("Incomplete", "Unverified", "Complete", NA, "Incomplete", "Unverified", "Complete")
  )
})

test_that("dictionary column aliases normalize to the internal metadata schema", {
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
    `Branching Logic (Show field only if...)` = c(
      NA,
      "[record_id] <> 'missing'"
    ),
    `Identifier?` = c(NA, NA)
  )

  project <- get_quality_project_data(list(data = tibble::tibble(record_id = "1", choice_field = "1"), metadata = metadata))
  expect_equal(
    project$metadata$field_name,
    c("record_id", "choice_field")
  )
  expect_equal(project$metadata$required_field, c(TRUE, FALSE))
  expect_equal(
    get_choice_rows(project$metadata)$field_name,
    c("choice_field", "choice_field")
  )
  expect_equal(get_branching_metadata(project)$referenced_fields, "record_id")
})
