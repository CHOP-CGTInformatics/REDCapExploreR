test_that("relationship inputs require exact fields and complete definitions", {
  book <- get_weight_codebook()
  expect_error(field_relationship_summary(NULL, book), "one nonblank")
  expect_error(field_relationship_summary(NA_character_, book), "one nonblank")
  expect_error(field_relationship_summary("", book), "one nonblank")
  expect_error(field_relationship_summary(c("weight_kg", "bmi"), book), "one nonblank")
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

test_that("unrelated blank fields increase inventory counts without adding relationships or issues", {
  book <- get_weight_codebook()
  before <- field_relationship_summary("weight_kg", book, include_inspection_log = TRUE)
  extra <- tibble::tibble(field_name = c("unused_a", "unused_b"), form_name = "main", field_type = "text", field_label = "Unused")
  expanded <- get_test_codebook(dplyr::bind_rows(book$definitions$fields, extra))
  after <- field_relationship_summary("weight_kg", expanded, include_inspection_log = TRUE)
  expect_equal(after$references, before$references)
  expect_equal(after$issues, before$issues)
  expect_equal(after$coverage$metadata_column, before$coverage$metadata_column)
  field_locations <- c("branching_logic", "field_annotation", "field_label", "field_note", "section_header")
  expect_equal(
    after$coverage$location_count[after$coverage$metadata_column %in% field_locations],
    before$coverage$location_count[before$coverage$metadata_column %in% field_locations] + 2L
  )
  unchanged_locations <- c("choice_label", "select_choices_or_calculations", "custom_record_label")
  expect_equal(
    after$coverage$location_count[after$coverage$metadata_column %in% unchanged_locations],
    before$coverage$location_count[before$coverage$metadata_column %in% unchanged_locations]
  )
  added <- dplyr::filter(after$inspection_log, .data$dependent_name %in% extra$field_name)
  expect_setequal(added$dependent_name, extra$field_name)
  expect_true(all(added$row_kind == "source"))
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
  book <- get_weight_codebook()
  expect_error(field_relationship_summary("weight_kg", book, include_diagnostics = NA), "TRUE or FALSE")
  expect_error(field_relationship_summary("weight_kg", book, include_diagnostics = 1), "TRUE or FALSE")
  expect_error(field_relationship_summary("weight_kg", book, include_diagnostics = c(TRUE, FALSE)), "TRUE or FALSE")
})

test_that("project coverage includes all interpretation limits independently of the field", {
  weight <- field_relationship_summary("weight_kg", mock_codebook)
  age <- field_relationship_summary("age", mock_codebook)
  expect_equal(weight$coverage, age$coverage)
  limits <- dplyr::filter(weight$coverage, .data$scope == "project_notes")
  expect_equal(nrow(limits), 1L)
  expect_equal(limits$location_count, 3L)
  expect_equal(limits$reference_count, 3L)
  expect_equal(nrow(age$issues), 0L)
  expect_equal(nrow(weight$issues), 3L)
  expect_setequal(weight$issues$dependent_name, c("diagnostic_example", "weight_at_entry"))
})

test_that("projects without interpretation limits omit the project notes aggregate", {
  report <- field_relationship_summary("weight_kg", get_weight_codebook())
  expect_false("project_notes" %in% report$coverage$scope)
  expect_equal(nrow(report$issues), 0L)
})

test_that("project diagnostics retain interpretation issues without the inspection inventory", {
  weight <- field_relationship_summary("weight_kg", mock_codebook, include_diagnostics = TRUE)
  age <- field_relationship_summary("age", mock_codebook, include_diagnostics = TRUE)
  expect_equal(weight$diagnostics, age$diagnostics)
  expect_equal(nrow(age$issues), 0L)
  expect_equal(nrow(age$diagnostics), 3L)
  expect_setequal(age$diagnostics$status, c("unresolved", "unclassified", "context_dependent"))
  expect_true(all(nzchar(age$diagnostics$expression)))
  expect_true(all(nzchar(age$diagnostics$status_note)))
  expect_false("row_kind" %in% names(age$diagnostics))
  expect_false("inspection_log" %in% names(age))
  expect_equal(weight$issues, weight$diagnostics)
})

test_that("project diagnostics include malformed sources without reference tokens", {
  book <- get_weight_codebook()
  book$definitions$fields$branching_logic[10] <- "[weight_kg > 10"
  report <- field_relationship_summary("height_cm", book, include_diagnostics = TRUE)
  issue <- dplyr::filter(report$diagnostics, .data$dependent_name == "symptoms")
  expect_equal(issue$status, "unclassified")
  expect_equal(issue$detail, "Unmatched reference brackets.")
  expect_equal(issue$expression, "[weight_kg > 10")
  expect_length(issue$occurrence_ids[[1]], 0)
  expect_false("symptoms" %in% report$issues$dependent_name)
})

test_that("the full inspection log is an independent opt-in", {
  ordinary <- field_relationship_summary("age", mock_codebook)
  log <- field_relationship_summary("age", mock_codebook, include_inspection_log = TRUE)
  both <- field_relationship_summary("age", mock_codebook, include_diagnostics = TRUE, include_inspection_log = TRUE)
  expect_false("diagnostics" %in% names(log))
  expect_true(all(c("source", "reference", "scope") %in% log$inspection_log$row_kind))
  expect_true(all(c("blank", "inspected", "not_inspected") %in% log$inspection_log$status))
  expect_equal(log$inspection_log, both$inspection_log)
  expect_equal(ordinary$coverage, both$coverage)
  expect_equal(ordinary$issues, both$issues)
  expect_error(field_relationship_summary("age", mock_codebook, include_inspection_log = NA), "include_inspection_log.*TRUE or FALSE")
  expect_error(field_relationship_summary("age", mock_codebook, include_inspection_log = 1), "include_inspection_log.*TRUE or FALSE")
  expect_error(
    field_relationship_summary("age", mock_codebook, include_inspection_log = c(TRUE, FALSE)),
    "include_inspection_log.*TRUE or FALSE"
  )
})

test_that("checkbox explanations identify raw option codes and labels without inventing other links", {
  report <- field_relationship_summary("symptoms", get_checkbox_relationship_codebook())
  grade <- dplyr::filter(report$summary, .data$dependent_name == "high_grade")
  expect_equal(grade$reference_choice, "fever")
  expect_equal(grade$reference_choice_label, "Fever")
  expect_equal(grade$explanation, "high_grade references [symptoms(fever)] (Fever) in its branching logic.")
  expect_equal(grade$expression, "[symptoms(fever)] = '1'")
  expect_false(any(report$references$reference_choice == "cough", na.rm = TRUE))
  expect_true("high_grade" %in% report$downstream$item_name)
  general <- dplyr::filter(report$summary, .data$dependent_name == "display")
  expect_true(is.na(general$reference_choice))
  expect_equal(general$explanation, "display pipes data or labels from symptoms.")
})

test_that("checkbox explanations preserve numeric codes and unknown options", {
  book <- get_checkbox_relationship_codebook()
  metadata <- book$definitions$fields
  metadata$select_choices_or_calculations[metadata$field_name == "symptoms"] <- "01, Fever | 02, Cough"
  metadata$select_choices_or_calculations[metadata$field_name == "prompt"] <- "1, Details for [symptoms(01)] | 2, Other"
  metadata$branching_logic[metadata$field_name == "high_grade"] <- "[symptoms(01)] = '1'"
  book <- get_codebook(get_codebook_project(list(metadata = metadata)))
  report <- field_relationship_summary("symptoms", book)
  grade <- dplyr::filter(report$summary, .data$dependent_name == "high_grade")
  expect_equal(grade$reference_choice, "01")
  expect_equal(grade$reference_choice_label, "Fever")
  expect_equal(grade$explanation, "high_grade references [symptoms(01)] (Fever) in its branching logic.")
  book$definitions$fields$branching_logic[book$definitions$fields$field_name == "high_grade"] <- "[symptoms(missing)] = '1'"
  unknown <- field_relationship_summary("symptoms", book)$summary |>
    dplyr::filter(.data$dependent_name == "high_grade")
  expect_equal(unknown$reference_choice, "missing")
  expect_true(is.na(unknown$reference_choice_label))
  expect_equal(unknown$resolution, "unresolved")
  expect_match(unknown$explanation, "[symptoms(missing)]", fixed = TRUE)
  expect_false(stringr::str_detect(unknown$explanation, "Fever"))
})
