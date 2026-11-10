test_that("relationship inputs require exact fields and complete definitions", {
  book <- get_weight_codebook()
  purrr::walk(list(NULL, NA_character_, "", c("weight_kg", "bmi")), \(field) {
    expect_error(field_relationship_summary(field, book), "one nonblank")
  })
  expect_error(field_relationship_summary("Weight", book), "exact field name")
  expect_error(field_relationship_summary("symptoms___1", book), "checkbox parent")
  expect_s3_class(field_relationship_summary("symptoms", book), "redcap_field_relationship_summary")
  old <- book
  old$definitions <- NULL
  expect_error(field_relationship_summary("weight_kg", old), "Rebuild")
  expect_s3_class(view_codebook(old), "shiny.tag.list")
  book$definitions$fields$field_name[2] <- "record_id"
  expect_error(field_relationship_summary("record_id", book), "unique, nonblank")
})

test_that("weight relationships preserve direction and indirect evidence", {
  report <- field_relationship_summary("weight_kg", get_weight_codebook())
  expect_named(report, c("field", "upstream", "references", "downstream", "summary", "issues", "coverage"))
  expect_equal(report$upstream$item_name, "record_id")
  expect_equal(report$downstream$item_name, c("bmi", "starting_weight", "visit_summary", "panel", "review_required", "review_notes"))
  expect_equal(report$downstream$depth, c(1L, 1L, 1L, 1L, 2L, 3L))
  expect_false("height_cm" %in% report$references$source_field)
  expect_equal(report$field$field_annotation, "@READONLY")
  notes <- dplyr::filter(report$downstream, .data$item_name == "review_notes")
  expect_equal(notes$field_path[[1]], c("weight_kg", "bmi", "review_required", "review_notes"))
  expect_true(all(notes$path[[1]] %in% report$references$occurrence_id))
  expect_true(any(grepl("assigning its default value", report$summary$explanation, fixed = TRUE)))
  expect_false(any(grepl("delete|risk score|will change", report$summary$explanation)))
  expect_output(print(report), "4 direct items, 2 indirect items")
})

test_that("repeated occurrences and alternative paths do not duplicate items", {
  book <- get_weight_codebook()
  book$definitions$fields$select_choices_or_calculations[4] <- "[weight_kg] + [weight_kg]"
  book$definitions$fields$branching_logic[6] <- "[bmi] > 0 or [review_required] = 1"
  report <- field_relationship_summary("weight_kg", book)
  expect_equal(sum(report$references$dependent_name == "bmi"), 2)
  expect_equal(sum(report$downstream$item_name == "bmi"), 1)
  expect_equal(report$downstream$depth[report$downstream$item_name == "review_notes"], 2L)
  expect_equal(sum(report$references$dependent_name == "review_notes"), 2)
})

test_that("cycles and self references terminate while retaining edges", {
  book <- get_weight_codebook()
  book$definitions$fields$branching_logic[2] <- "[review_required] > 0 and [weight_kg] > 0"
  report <- field_relationship_summary("weight_kg", book)
  expect_equal(nrow(report$downstream), 6)
  expect_true(any(report$references$source_field == "weight_kg" & report$references$dependent_name == "weight_kg"))
  expect_true("both" %in% report$references$traversal_role)
  expect_false("weight_kg" %in% report$downstream$item_name)
})

test_that("disconnected fields have typed empty relationships and explicit coverage", {
  report <- field_relationship_summary("symptoms", get_weight_codebook())
  expect_equal(nrow(report$references), 0)
  expect_type(report$references$occurrence_id, "integer")
  expect_type(report$upstream$connection, "character")
  expect_equal(nrow(report$summary), 0)
  expect_output(print(report), "No references to symptoms were found in the inspected metadata")
  expect_true(any(report$coverage$metadata_column == "field_note" & report$coverage$status == "unavailable"))
  expect_true(any(report$coverage$status == "not_inspected"))
})

test_that("unknown references are visible even outside the selected graph", {
  book <- get_weight_codebook()
  book$definitions$fields$branching_logic[10] <- "[missing_field] = '1'"
  book$definitions$fields$field_annotation[6] <- "@UNKNOWN([weight_kg])"
  report <- field_relationship_summary("height_cm", book, include_diagnostics = TRUE)
  expect_true(any(report$diagnostics$status == "unresolved" & report$diagnostics$dependent_name == "symptoms"))
  expect_true(any(report$diagnostics$status == "unclassified" & report$diagnostics$dependent_name == "review_notes"))
  expect_true(any(report$coverage$scope == "project_notes"))
  weight <- field_relationship_summary("weight_kg", book)
  expect_true(any(weight$references$resolution == "unclassified"))
})

test_that("relationship reports use only the supplied codebook", {
  testthat::local_mocked_bindings(
    pull_redcap_project = function(...) stop("No records"),
    pull_redcap_codebook = function(...) stop("No API")
  )
  book <- get_weight_codebook()
  expect_s3_class(field_relationship_summary("weight_kg", book), "redcap_field_relationship_summary")
})

test_that("incomplete references disclose source coverage limits", {
  book <- get_weight_codebook()
  book$definitions$fields$branching_logic[6] <- "[weight_kg > 10"
  issues <- field_relationship_summary("review_notes", book)$issues
  expect_true(any(issues$status == "unclassified"))
})

test_that("empty references use the same column schema as populated reports", {
  book <- get_weight_codebook()
  expect_named(
    field_relationship_summary("symptoms", book)$references,
    names(field_relationship_summary("weight_kg", book)$references)
  )
})

test_that("status notes retain specific context and extraction reasons", {
  book <- get_weight_codebook()
  book$definitions$fields$branching_logic[6] <- "[previous-event-name][weight_kg] > 5"
  report <- field_relationship_summary("weight_kg", book)
  notes <- report$summary$status_note[report$summary$dependent_name == "review_notes"]
  expect_match(notes, "Context dependent: Event is selected dynamically.", fixed = TRUE)
  expect_equal(
    report$references$status_note[report$references$resolution == "resolved"],
    rep("", sum(report$references$resolution == "resolved"))
  )
  book$definitions$fields$field_annotation[6] <- "@UNKNOWN([weight_kg])"
  report <- field_relationship_summary("weight_kg", book)
  expect_true(any(grepl("Unclassified reference: Unsupported", report$summary$status_note, fixed = TRUE)))
})

test_that("coverage stays compact as unrelated blank fields are added", {
  book <- get_weight_codebook()
  extra <- tibble::tibble(
    field_name = paste0("unused_", 1:1000), form_name = "main",
    field_type = "text", field_label = "Unused"
  )
  large <- get_codebook(get_codebook_project(list(metadata = dplyr::bind_rows(book$definitions$fields, extra))))
  report <- field_relationship_summary("weight_kg", large)
  detailed <- field_relationship_summary("weight_kg", large, include_diagnostics = TRUE)
  expect_lt(nrow(report$coverage), 20)
  expect_false("diagnostics" %in% names(report))
  expect_gt(nrow(detailed$diagnostics), 5000)
  expect_equal(report$coverage, detailed$coverage)
  expect_true(all(nchar(report$coverage$examples) < 200))
})

test_that("related issues retain evidence without project-wide diagnostics", {
  report <- field_relationship_summary("weight_kg", mock_codebook)
  missing <- dplyr::filter(report$issues, .data$status == "unresolved")
  expect_match(missing$expression, "retired_weight", fixed = TRUE)
  expect_match(missing$detail, "Field identity could not be resolved", fixed = TRUE)
  expect_false("diagnostics" %in% names(report))
  expect_false(any(report$references$source_field == "retired_weight"))
  expect_false("related_notes" %in% report$coverage$scope)
  expect_true(all(report$coverage$scope %in% c("sources", "project_notes", "boundaries")))
})

test_that("summary preserves metadata locations and counts repeated tokens within expressions", {
  book <- get_weight_codebook()
  fields <- book$definitions$fields
  fields$field_note <- ifelse(fields$field_name == "visit_summary", "Again [weight_kg] and [weight_kg]", NA_character_)
  book$definitions$fields <- fields
  book$definitions$source_columns$fields <- names(fields)
  report <- field_relationship_summary("weight_kg", book)
  summary <- dplyr::filter(report$summary, .data$dependent_name == "visit_summary")
  expect_setequal(summary$metadata_column, c("field_label", "field_note"))
  expect_equal(summary$occurrence_count, c(1L, 2L))
  expect_equal(summary$expression, c("Weight: [weight_kg] kg", "Again [weight_kg] and [weight_kg]"))
})

test_that("mixed reference chains explain each step without predicting value changes", {
  metadata <- tibble::tibble(
    field_name = c("a", "b", "c"), form_name = "main", field_type = "text",
    field_label = c("A", "B pipes [a]", "C"), branching_logic = c(NA, NA, "[b] > 0")
  )
  report <- field_relationship_summary("a", get_codebook(get_codebook_project(list(metadata = metadata))))
  chain <- report$downstream$reference_chain[report$downstream$item_name == "c"]
  expect_equal(chain, "a -[Piping]-> b -[Branching logic]-> c")
  expect_match(as.character(view_field_relationship_summary(report)), "do not establish that values or behavior will change", fixed = TRUE)
  upstream <- field_relationship_summary("c", get_codebook(get_codebook_project(list(metadata = metadata))))
  expect_equal(upstream$upstream$reference_chain[upstream$upstream$item_name == "a"], "c <-[Branching logic]- b <-[Piping]- a")
})

test_that("no-argument action tags do not create interpretation issues", {
  report <- field_relationship_summary("weight_kg", get_weight_codebook())
  expect_equal(nrow(report$issues), 0)
})

test_that("the diagnostics flag requires one logical value", {
  purrr::walk(list(NA, 1, c(TRUE, FALSE)), \(flag) {
    expect_error(field_relationship_summary("weight_kg", get_weight_codebook(), include_diagnostics = flag), "TRUE or FALSE")
  })
})
