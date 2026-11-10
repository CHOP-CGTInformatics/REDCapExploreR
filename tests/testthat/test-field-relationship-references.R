test_that("expression references exclude literals and preserve exact spans", {
  text <- "[weight_kg] + [weight_kg] + '[height_cm]' + \"O\\\"[bmi]\""
  tokens <- get_relationship_tokens(text)
  expect_equal(tokens$reference_text, rep("[weight_kg]", 2))
  expect_equal(substring(text, tokens$start, tokens$end), tokens$reference_text)
  expect_equal(get_relationship_tokens("[weight_kg_extra]")$reference_text, "[weight_kg_extra]")
})

test_that("display sources discover quoted piping and embedding", {
  tokens <- get_relationship_tokens("<span title='[weight_kg:value]'>{review_notes}</span>", "text")
  expect_equal(tokens$reference_text, c("[weight_kg:value]", "{review_notes}"))
})

test_that("annotation arguments distinguish defaults, calculations and ambiguity", {
  source <- get_relationship_sources(get_weight_codebook()) |>
    dplyr::filter(.data$dependent_name == "starting_weight", .data$metadata_column == "field_annotation")
  cases <- tibble::tribble(
    ~expression, ~relationship, ~interpretation,
    "@DEFAULT='[weight_kg]'", "initial_value", "recognized",
    "@CALCTEXT([weight_kg] * 2)", "calculation_input", "recognized",
    "@CALCDATE([weight_kg], 1, 'd')", "calculation_input", "recognized",
    "@CALCTEXT('[weight_kg]')", "calculation_input", "unclassified",
    "@UNKNOWN([weight_kg])", "unclassified", "unclassified",
    "@DEFAULT('[weight_kg]'", "initial_value", "unclassified"
  )
  purrr::walk(seq_len(nrow(cases)), \(i) {
    source$expression <- cases$expression[[i]]
    references <- get_relationship_source_references(source)
    expect_equal(references$relationship, cases$relationship[[i]], info = source$expression)
    expect_equal(references$interpretation, cases$interpretation[[i]], info = source$expression)
  })
})

test_that("nested IF annotations retain condition and branch relationships", {
  source <- get_relationship_sources(get_weight_codebook()) |>
    dplyr::filter(.data$dependent_name == "starting_weight", .data$metadata_column == "field_annotation")
  source$expression <- "@IF([record_id] = '1', '@DEFAULT=\"[weight_kg]\"', '@HIDDEN')"
  references <- get_relationship_source_references(source)
  expect_equal(references$reference_text, c("[record_id]", "[weight_kg]"))
  expect_equal(references$relationship, c("annotation_condition", "initial_value"))
  expect_equal(references$interpretation, rep("recognized", 2))
})

test_that("qualifiers resolve field identity independently of context", {
  book <- get_weight_codebook()
  source <- get_relationship_sources(book) |>
    dplyr::filter(.data$dependent_name == "bmi", .data$context == "calculation")
  source$expression <- paste("[baseline_arm_1][weight_kg][2]", "[previous-event-name][weight_kg][last-instance]",
    "[unknown_arm_1][weight_kg]", "[symptoms(1)]", "[symptoms(9)]", "[weight_kg:unknown]",
    sep = " + "
  )
  raw <- get_relationship_source_references(source)
  references <- dplyr::bind_rows(purrr::map(seq_len(nrow(raw)), \(i) get_resolved_relationship(raw[i, ], book)))
  expect_equal(references$source_field, c(rep("weight_kg", 3), rep("symptoms", 2), "weight_kg"))
  expect_equal(references$resolution, c("resolved", "context_dependent", "unresolved", "resolved", "unresolved", "unresolved"))
  expect_equal(references$event[1:2], c("baseline_arm_1", "previous-event-name"))
  expect_equal(references$instance[1:2], c("2", "last-instance"))
  expect_equal(references$reference_choice[4:5], c("1", "9"))
})

test_that("piping and embedding retain their metadata owner", {
  book <- get_weight_codebook()
  fields <- book$definitions$fields
  fields$field_note <- ifelse(fields$field_name == "bmi", "Uses [weight_kg]", NA_character_)
  fields$section_header <- ifelse(fields$field_name == "review_notes", "Weight [weight_kg]", NA_character_)
  book$definitions$fields <- fields
  book$definitions$source_columns$fields <- names(fields)
  book$definitions$project <- tibble::tibble(custom_record_label = "[weight_kg]")
  book$definitions$source_columns$project <- "custom_record_label"
  report <- field_relationship_summary("weight_kg", book)
  expect_true(all(c("field_note", "section_header", "custom_record_label") %in% report$references$metadata_column))
  expect_equal(report$references$relationship[report$references$dependent_name == "panel"], "embedded_field")
  expect_equal(report$downstream$item_kind[report$downstream$item_name == "custom_record_label"], "project_setting")
})

test_that("choice label references retain whitespace and choice ownership", {
  metadata <- tibble::tibble(
    field_name = c("weight_kg", "question"), form_name = "main",
    field_type = c("text", "radio"), field_label = c("Weight", "Question"),
    select_choices_or_calculations = c(NA, "1, Current [weight_kg] | 2, Other")
  )
  book <- get_codebook(get_codebook_project(list(metadata = metadata)))
  references <- field_relationship_summary("weight_kg", book)$references
  expect_equal(references$metadata_column, "choice_label")
  expect_equal(references$choice_code, "1")
  expect_equal(references$expression, " Current [weight_kg] ")
})

test_that("tag-like text inside calculation literals is not a default relationship", {
  source <- get_relationship_sources(get_weight_codebook()) |>
    dplyr::filter(.data$dependent_name == "starting_weight", .data$metadata_column == "field_annotation")
  source$expression <- "@CALCTEXT('@DEFAULT=\"[weight_kg]\"')"
  references <- get_relationship_source_references(source)
  expect_equal(references$interpretation, "unclassified")
  expect_false("initial_value" %in% references$relationship)
})

test_that("adjacent display pipes remain separate field references", {
  book <- get_weight_codebook()
  book$definitions$fields$field_label[8] <- "[weight_kg] [height_cm][bmi]"
  references <- field_relationship_summary("weight_kg", book)$references
  pipes <- dplyr::filter(references, .data$dependent_name == "visit_summary")
  expect_equal(pipes$reference_text, c("[weight_kg]", "[bmi]"))
  expect_equal(pipes$resolution, rep("resolved", 2))
})

test_that("field matching is independent of unsupported qualifiers", {
  book <- get_weight_codebook()
  book$definitions$fields$field_label[8] <- "[symptoms:checked:value]; [symptoms:unchecked:value]; [weight_kg:unknown]; [missing_field]"
  report <- field_relationship_summary("symptoms", book)
  pipes <- dplyr::filter(report$references, .data$source_field == "symptoms")
  expect_equal(pipes$resolution, c("resolved", "resolved"))
  expect_true(all(pipes$field_matched))
  expect_true("visit_summary" %in% report$downstream$item_name)
  report <- field_relationship_summary("weight_kg", book)
  pipe <- dplyr::filter(report$references, .data$modifier == "unknown")
  expect_true(pipe$field_matched)
  expect_equal(pipe$resolution, "unresolved")
  expect_true("visit_summary" %in% report$downstream$item_name)
  other <- field_relationship_summary("visit_summary", book)
  expect_false(other$references$field_matched[other$references$source_field == "missing_field"])
})

test_that("embedding across instruments has a note without hiding the known relationship", {
  book <- get_weight_codebook()
  book$definitions$fields$form_name[9] <- "other"
  report <- field_relationship_summary("weight_kg", book)
  embedding <- dplyr::filter(report$references, .data$relationship == "embedded_field")
  expect_equal(embedding$resolution, "unresolved")
  expect_match(embedding$status_note, "same instrument", fixed = TRUE)
  expect_true("panel" %in% report$downstream$item_name)
})

test_that("direction describes self references and cycles independently of interpretation", {
  book <- get_weight_codebook()
  book$definitions$fields$branching_logic[2] <- "[review_required] > 0 and [weight_kg] > 0"
  report <- field_relationship_summary("weight_kg", book)
  self <- dplyr::filter(report$references, .data$source_field == "weight_kg", .data$dependent_name == "weight_kg")
  expect_equal(self$traversal_role, "self")
  expect_true("both" %in% report$references$traversal_role)
  book <- get_weight_codebook()
  book$definitions$fields$field_annotation[6] <- "@UNKNOWN([symptoms])"
  report <- field_relationship_summary("symptoms", book)
  candidate <- dplyr::filter(report$references, .data$interpretation == "unclassified")
  expect_equal(candidate$traversal_role, "downstream")
  expect_false("review_notes" %in% report$downstream$item_name)
})
