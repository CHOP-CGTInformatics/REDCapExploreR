test_that("references show all returned rows without a default filter", {
  report <- field_relationship_summary("weight_kg", get_weight_codebook())
  tables <- get_relationship_viewer_tables(report)
  expect_equal(nrow(tables$references$data), nrow(report$references))
  expect_equal(tables$references$data[["Reference text"]], report$references$reference_text)
  expect_false("view" %in% names(tables$references$data))
  expect_false("Occurrence ID" %in% names(tables$references$data))
  widget <- get_metadata_datatable(tibble::tibble(view = c("Direct outgoing", "Other relationships")), 25)
  expect_null(widget$x$options$searchCols)
  html <- as.character(view_field_relationship_summary(report))
  expect_match(html, "Shows every returned reference", fixed = TRUE)
  expect_match(html, "Coverage", fixed = TRUE)
  expect_error(view_field_relationship_summary(list()), "must be returned")
  expect_error(view_field_relationship_summary(report, page_length = Inf), "positive integer")
})

test_that("viewer labels relationship types and hides routine interpretation statuses", {
  report <- field_relationship_summary("weight_kg", get_weight_codebook())
  data <- get_relationship_viewer_tables(report)$summary$data
  expect_true("Calculation" %in% data[["Relationship type"]])
  expect_true("Explanation" %in% names(data))
  expect_true(all(data[["Interpretation note"]] == ""))
  expect_false("resolution" %in% names(data))
  expect_true(all(report$summary$resolution == "resolved"))
})

test_that("detailed diagnostics appear only when explicitly requested", {
  book <- get_weight_codebook()
  ordinary <- field_relationship_summary("weight_kg", book)
  detailed <- field_relationship_summary("weight_kg", book, include_diagnostics = TRUE)
  expect_false("diagnostics" %in% names(get_relationship_viewer_tables(ordinary)))
  expect_true("diagnostics" %in% names(get_relationship_viewer_tables(detailed)))
  expect_match(as.character(view_field_relationship_summary(detailed)), "Diagnostics", fixed = TRUE)
  expect_equal(detailed$coverage, ordinary$coverage)
})

test_that("viewer roots have independent navigation identifiers", {
  report <- field_relationship_summary("weight_kg", get_weight_codebook())
  a <- as.character(view_field_relationship_summary(report))
  b <- as.character(view_codebook(get_weight_codebook()))
  id <- function(html) regmatches(html, regexpr('id="redcap-viewer-[^"]+"', html))
  expect_false(id(a) == id(b))
  expect_match(a, 'tabindex="0"', fixed = TRUE)
  expect_match(a, "aria-labelledby=", fixed = TRUE)
})

test_that("saved relationship HTML includes escaped metadata and local dependencies", {
  book <- get_weight_codebook()
  book$definitions$fields$field_annotation[2] <- '<img src=x onerror="alert(1)">'
  report <- field_relationship_summary("weight_kg", book)
  file <- tempfile(fileext = ".html")
  expect_equal(save_field_relationship_summary(report, file), file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "data-redcap-codebook-source", fixed = TRUE)
  expect_match(html, "&lt;img", fixed = TRUE)
  expect_false(grepl('<img src=x onerror="alert(1)">', html, fixed = TRUE))
  expect_false(grepl("<script[^>]+ src=", html))
})

test_that("empty viewer describes the inspected scope", {
  html <- as.character(view_field_relationship_summary(field_relationship_summary("symptoms", get_weight_codebook())))
  expect_match(html, "No references to symptoms were found in the inspected metadata", fixed = TRUE)
})

test_that("viewer sections and print use report terms with concise item tables", {
  report <- field_relationship_summary("weight_kg", mock_codebook)
  tables <- get_relationship_viewer_tables(report)
  expect_equal(vapply(tables, `[[`, character(1), "title"), c(
    definition = "Field", upstream = "Upstream", downstream = "Downstream",
    summary = "Summary", references = "References", issues = "Interpretation issues", coverage = "Coverage"
  ))
  expect_output(print(report), "Upstream: 1 item")
  expect_output(print(report), "Downstream: 6 direct items, 1 indirect item")
  expect_output(print(report), "Coverage: project-wide")
  expect_lte(ncol(tables$downstream$data), 8)
  expect_false(any(c("field_name", "item_name", "path", "depth", "Occurrence ID") %in% names(tables$downstream$data)))
  expect_equal(nrow(tables$downstream$data), nrow(report$downstream))
  expect_true(any(nzchar(tables$references$data[["Interpretation note"]])))
  expect_true("Project reference occurrences" %in% names(tables$coverage$data))
  expect_match(paste(tables$issues$data[["Original expression"]], collapse = " "), "retired_weight", fixed = TRUE)
})

test_that("viewer uses REDCap labels and shows specific action tags", {
  report <- field_relationship_summary("weight_kg", mock_codebook)
  data <- get_relationship_viewer_tables(report)$summary$data
  expect_true(all(c(
    "Branching logic", "Piping", "Field embedding", "Default value (@DEFAULT)",
    "Action-tag condition (@IF)"
  ) %in% data[["Relationship type"]]))
  expect_true("Field annotation" %in% data[["Metadata location"]])
  expect_false(any(c("Evidence path", "Occurrence IDs", "Direction") %in% names(data)))
})
