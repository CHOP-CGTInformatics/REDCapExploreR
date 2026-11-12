#' @title View a field relationship summary
#' @description Displays definitions, references, connections, and coverage as
#'   searchable HTML tables with original expressions and evidence paths.
#' @param relationships A report from [field_relationship_summary()].
#' @param page_length Number of rows shown per table page.
#' @returns An `htmltools` browsable HTML object.
#' @examples
#' relationships <- field_relationship_summary("weight_kg", mock_codebook)
#' \dontrun{
#' view_field_relationship_summary(relationships)
#' }
#' @export
view_field_relationship_summary <- function(relationships, page_length = 25) {
  if (!inherits(relationships, "redcap_field_relationship_summary")) {
    cli_abort("{.arg relationships} must be returned by {.fun field_relationship_summary}.")
  }
  get_validate_page_length(page_length)
  viewer_id <- basename(tempfile("redcap-viewer-"))
  tables <- get_relationship_viewer_tables(relationships)
  browsable(tagList(
    get_metadata_viewer_deps(),
    tags$main(
      id = viewer_id, class = "redcap-codebook-viewer",
      tags$header(
        class = "redcap-codebook-header",
        tags$h1(paste("Field relationships:", relationships$field$field_name)),
        tags$p("Explore upstream references and downstream uses. Matched references do not validate REDCap configuration."),
        if (nrow(relationships$references) == 0) {
          tags$p(paste(
            "No references to", relationships$field$field_name, "were found in the inspected metadata."
          ))
        },
        tags$p(paste(
          "Indirect connections identify configuration to review; they do not establish that values or behavior will change.",
          "Reference chains are at field level; event and repeating-instance continuity is not established."
        ))
      ),
      get_metadata_viewer_navigation(tables, viewer_id),
      get_metadata_viewer_sections(tables, page_length, viewer_id)
    )
  ))
}

#' @title Save a field relationship viewer
#' @description Writes a single HTML file with local dependencies inlined for
#'   offline exploration and sharing.
#' @param relationships A report from [field_relationship_summary()].
#' @param file Output HTML file path; its directory must exist.
#' @param page_length Number of rows shown per table page.
#' @returns The output path, invisibly.
#' @export
save_field_relationship_summary <- function(relationships, file, page_length = 25) {
  get_validate_html_file(file)
  viewer <- view_field_relationship_summary(relationships, page_length)
  get_save_metadata_single_file(viewer, file)
  invisible(file)
}

#' @title Prepare relationship viewer sections
#' @description Shows all references with readable connection types, notes, and
#'   a compact description of scope.
#' @param relationships A field relationship report.
#' @returns Section specifications for the shared metadata viewer.
#' @noRd
get_relationship_viewer_tables <- function(relationships) {
  definition <- tibble(
    attribute = names(relationships$field),
    value = map_chr(relationships$field, \(value) paste(get_codebook_viewer_cell(value), collapse = ", "))
  )
  tables <- list(
    definition = list(
      title = "Field", data = definition,
      description = "Selected field definition, field annotations, and instrument/event context."
    ),
    upstream = list(
      title = "Upstream", data = get_relationship_viewer_items(relationships$upstream),
      description = "Fields referenced by this field, directly or through other references. Arrows point toward the referencing item."
    ),
    downstream = list(
      title = "Downstream", data = get_relationship_viewer_items(relationships$downstream),
      description = "Fields and project settings that reference this field, directly or through other references."
    ),
    summary = list(
      title = "Summary", data = get_relationship_viewer_evidence(relationships$summary, summary = TRUE),
      description = "One explanation per referencing item, metadata location, expression, and reference interpretation."
    ),
    references = list(
      title = "References", data = get_relationship_viewer_evidence(relationships$references),
      description = "Shows every returned reference occurrence with its original text. Filters apply only when you choose them."
    )
  )
  if (nrow(relationships$issues) > 0) {
    tables$issues <- list(
      title = "Interpretation issues",
      data = relationships$issues |>
        mutate(
          .keep = "none",
          Item = .data$dependent_name,
          `Metadata location` = get_relationship_location_label(.data$metadata_column),
          `Choice code` = .data$choice_code,
          `Original expression` = .data$expression,
          `Interpretation note` = .data$status_note
        ),
      description = paste(
        "Issues involving the selected field or related items, including references outside the displayed chains.",
        "These are interpretation limits, not a count of invalid REDCap settings."
      )
    )
  }
  tables$coverage <- list(
    title = "Coverage",
    data = relationships$coverage |>
      mutate(
        .keep = "none",
        `Inspection scope` = case_when(
          .data$scope == "sources" ~ "Project sources",
          .data$scope == "project_notes" ~ "Other project interpretation limits",
          TRUE ~ "Excluded configuration"
        ), `Metadata source` = get_relationship_location_label(.data$metadata_column),
        `Inspection status` = case_when(
          .data$status == "available" ~ "Available",
          .data$status == "partially_available" ~ "Partially available",
          .data$status == "unavailable" ~ "Unavailable",
          .data$status == "not_inspected" ~ "Not inspected",
          TRUE ~ "See explanation"
        ), `Project metadata locations` = .data$location_count,
        `Project reference occurrences` = .data$reference_count, Explanation = .data$note
      ),
    description = paste(
      "Project-wide source availability and exclusions, not counts for this field.",
      "Source counts and interpretation-limit counts overlap and must not be added together.",
      "Request include_diagnostics = TRUE for the full project inventory."
    )
  )
  if (!is.null(relationships$diagnostics)) {
    tables$diagnostics <- list(
      title = "Diagnostics", data = relationships$diagnostics,
      description = "Optional project-wide diagnostics, including blank metadata locations."
    )
  }
  tables
}

#' @title Present reference evidence without parser bookkeeping
#' @description Keeps technical IDs and qualifiers in the R output while showing
#'   the original expressions and human-readable interpretation notes.
#' @param data Reference occurrences or relationship explanations.
#' @param summary Show grouped explanations rather than individual tokens.
#' @returns A presentation table with unchanged row membership.
#' @noRd
get_relationship_viewer_evidence <- function(data, summary = FALSE) {
  out <- data |>
    mutate(
      relationship = get_relationship_type_label(.data$relationship, .data$action_tag),
      metadata_column = get_relationship_location_label(.data$metadata_column)
    )
  columns <- if (summary) {
    c(
      "dependent_name", "metadata_column", "choice_code", "relationship", "explanation",
      "expression", "occurrence_count", "status_note"
    )
  } else {
    c(
      "dependent_name", "source_field", "metadata_column", "choice_code", "relationship",
      "expression", "reference_text", "field_matched", "status_note"
    )
  }
  out <- out |> select(all_of(columns))
  labels <- c(
    dependent_name = "Referencing item", source_field = "Referenced field",
    relationship = "Relationship type", explanation = "Explanation",
    status_note = "Interpretation note", metadata_column = "Metadata location",
    expression = "Original expression", reference_text = "Reference text",
    occurrence_count = "Occurrences in expression", field_matched = "Field name matched",
    choice_code = "Choice code"
  )
  names(out) <- unname(labels[names(out)])
  out
}

#' @title Present a concise inventory of related items
#' @description Shows one identity, reference-chain types, and a readable path.
#' @param data Upstream or downstream items.
#' @returns A presentation table without duplicated field definitions.
#' @noRd
get_relationship_viewer_items <- function(data) {
  data |>
    mutate(
      .keep = "none",
      Item = .data$item_name,
      Kind = if_else(.data$item_kind == "field", "Field", "Project setting"),
      Instrument = .data$form_name,
      Label = .data$field_label,
      Connection = if_else(.data$depth == 1, "Direct", paste0("Indirect (", .data$depth, " steps)")),
      `Types along reference chain` = .data$relationship_types,
      `Reference chain` = .data$reference_chain,
      `Interpretation note` = .data$status_note
    )
}

#' @title Label REDCap metadata locations
#' @description Uses designer terminology for retained dictionary columns.
#' @param column Metadata column names.
#' @returns Readable metadata location labels.
#' @noRd
get_relationship_location_label <- function(column) {
  labels <- c(
    branching_logic = "Branching logic", select_choices_or_calculations = "Calculation",
    field_annotation = "Field annotation", field_label = "Field label", field_note = "Field note",
    section_header = "Section header", choice_label = "Choice label", custom_record_label = "Custom record label"
  )
  if_else(column %in% names(labels), unname(labels[column]), column)
}
