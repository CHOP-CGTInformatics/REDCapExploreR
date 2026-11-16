test_that("branching compares numeric values rather than their character ordering", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = as.character(1:4), age = c("2", "10", "20", "100"), limit = c("1", "20", "10", "200")),
    metadata = dplyr::mutate(get_test_metadata(c("record_id", "age", "limit")),
      text_validation_type_or_show_slider_number = c(NA, "integer", "integer")
    )
  ))
  expect_equal(get_branching_applicable_rows(project, "[age] > 5"), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(get_branching_applicable_rows(project, "[age] > [limit]"), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(get_branching_applicable_rows(project, "[age] >= '20'"), c(FALSE, FALSE, TRUE, TRUE))
  project$data$age <- c("-10.5", "-2.5", ".5", "10.5")
  project$metadata$text_validation_type_or_show_slider_number[2] <- "number"
  expect_equal(get_branching_applicable_rows(project, "[age] > -2.5"), c(FALSE, FALSE, TRUE, TRUE))
})

test_that("categorical branching preserves leading zero codes", {
  metadata <- get_test_metadata(c("record_id", "code"), types = c("text", "radio"))
  metadata$select_choices_or_calculations <- c(NA, "01, Zero one | 1, One | 2, Two")
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = as.character(1:3), code = c("01", "1", "2")), metadata = metadata
  ))
  expect_equal(get_branching_applicable_rows(project, "[code] = 1"), c(FALSE, TRUE, FALSE))
  expect_equal(get_branching_applicable_rows(project, "[code] = '01'"), c(TRUE, FALSE, FALSE))
})

test_that("quoted branching literals retain operators brackets and apostrophes", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = as.character(1:4), answer = c("and", "a=b", "x[y]", "O'Brien")),
    metadata = get_test_metadata(c("record_id", "answer"))
  ))
  expect_equal(get_branching_applicable_rows(project, "[answer] = 'and'"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(get_branching_applicable_rows(project, "[answer] = 'a=b'"), c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(get_branching_applicable_rows(project, "[answer] = 'x[y]'"), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(get_branching_applicable_rows(project, "[answer] = \"O'Brien\""), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("blank branching comparisons treat missing and empty text as blank", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = as.character(1:4), value = c(NA, "", "yes", "no")),
    metadata = get_test_metadata(c("record_id", "value"))
  ))
  expect_equal(get_branching_applicable_rows(project, "[value] = ''"), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(get_branching_applicable_rows(project, "[value] <> ''"), c(FALSE, FALSE, TRUE, TRUE))
})

test_that("compound branching respects parentheses and boolean precedence", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = as.character(1:4), age = c("2", "10", "20", "100")),
    metadata = dplyr::mutate(get_test_metadata(c("record_id", "age")),
      text_validation_type_or_show_slider_number = c(NA, "integer")
    )
  ))
  expect_equal(get_branching_applicable_rows(project, "([age] > 5 and [age] < 100) or [age] = 2"), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("unsupported branching remains assessable rather than excluding data", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = c("1", "2"), age = c("2", "10")),
    metadata = get_test_metadata(c("record_id", "age"))
  ))
  expect_equal(get_branching_applicable_rows(project, "[missing] = 1"), c(TRUE, TRUE))
  expect_equal(get_branching_applicable_rows(project, "[event_arm_1][age] > 5"), c(TRUE, TRUE))
  expect_equal(get_branching_applicable_rows(project, "datediff([age], 'today', 'y') > 1"), c(TRUE, TRUE))
  expect_equal(get_branching_applicable_rows(project, "[age] + 1 > 5"), c(TRUE, TRUE))
})

test_that("invalid numeric values do not become blank branching references", {
  project <- get_quality_project_data(list(
    data = tibble::tibble(record_id = c("1", "2"), value = c("bad", NA_character_)),
    metadata = dplyr::mutate(get_test_metadata(c("record_id", "value")),
      text_validation_type_or_show_slider_number = c(NA, "integer")
    )
  ))
  expect_equal(get_branching_applicable_rows(project, "[value] = ''"), c(FALSE, TRUE))
})
