#' @title Retain the complete relationship inspection log
#' @description Makes unavailable columns and out-of-scope configuration explicit.
#' @param sources Metadata locations and availability.
#' @param references All extracted project occurrences.
#' @param codebook A validated codebook.
#' @returns Source coverage and reference-level interpretation limits.
#' @noRd
get_relationship_inspection_log <- function(sources, references, codebook) {
  counts <- references |> count(.data$source_id, name = "reference_count")
  coverage <- sources |>
    left_join(counts, by = "source_id") |>
    mutate(
      reference_count = if_else(is.na(.data$reference_count), 0L, .data$reference_count),
      status = case_when(
        !.data$available ~ "unavailable",
        get_is_missing(.data$expression) ~ "blank",
        .data$context == "unsupported" ~ "not_inspected",
        TRUE ~ "inspected"
      ),
      detail = case_when(
        .data$status == "unavailable" ~ "Source column was not returned.",
        .data$status == "not_inspected" ~ "Setting text is retained but its behavior is not interpreted.",
        .data$status == "blank" ~ "Available source contains no expression.",
        TRUE ~ "References extracted without evaluating the expression."
      ),
      occurrence_id = NA_integer_, row_kind = "source"
    )
  source_issues <- map_chr(seq_len(nrow(sources)), \(i) {
    source <- sources[i, ]
    if (!source$available || get_is_missing(source$expression) || source$context == "unsupported") {
      return(NA_character_)
    }
    if (source$context == "annotation") {
      regions <- get_relationship_annotation_regions(source$expression)
      unknown <- regions$tag[map_lgl(seq_len(nrow(regions)), \(i) {
        !regions$tag[[i]] %in% c("@DEFAULT", "@CALCTEXT", "@CALCDATE", "@IF") &&
          nrow(get_relationship_tokens(substr(source$expression, regions$start[[i]], regions$end[[i]]), "text")) > 0
      })]
      if (length(unknown) > 0) {
        return(paste("Annotation behavior is not interpreted for", paste(unknown, collapse = ", ")))
      }
      if (any(!regions$complete & regions$tag %in% c("@DEFAULT", "@CALCTEXT", "@CALCDATE", "@IF"))) {
        return("Incomplete annotation argument.")
      }
    }
    text <- source$expression
    if (source$context %in% c("expression", "calculation")) {
      text <- str_remove_all(text, r"{'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"}")
    }
    text <- str_remove_all(text, r"{\[[^\[\]\r\n]+\](?:\s*\[[^\[\]\r\n]+\])*}")
    if (str_detect(text, "[\\[\\]]")) "Unmatched reference brackets." else NA_character_
  })
  partial <- !is.na(source_issues)
  coverage$status[partial] <- "unclassified"
  coverage$detail[partial] <- source_issues[partial]
  unresolved <- references |>
    filter(.data$resolution != "resolved") |>
    mutate(
      .keep = "none",
      source_id = .data$source_id, dependent_kind = .data$dependent_kind,
      dependent_name = .data$dependent_name, metadata_column = .data$metadata_column,
      choice_code = .data$choice_code, expression = .data$expression, context = .data$relationship,
      available = TRUE, reference_count = 1L, status = .data$resolution,
      detail = .data$reason, occurrence_id = .data$occurrence_id, row_kind = "reference"
    )
  outside <- tibble(
    source_id = NA_integer_, dependent_kind = "configuration",
    dependent_name = c("alerts", "survey_queues", "report_filters", "external_modules_and_scripts"),
    metadata_column = NA_character_, choice_code = NA_character_, expression = NA_character_,
    context = "unsupported", available = FALSE, reference_count = 0L, status = "not_inspected",
    detail = "Configuration is outside the inspected metadata.", occurrence_id = NA_integer_, row_kind = "scope"
  )
  bind_rows(coverage, unresolved, outside) |>
    mutate(definitions_version = codebook$definitions$schema_version, parser_version = 1L)
}

#' @title Summarize project-wide relationship inspection scope
#' @description Groups available sources and all project interpretation limits
#'   independently of the selected field.
#' @param inspection_log Complete project inspection log.
#' @returns Compact project-wide source counts, interpretation limits, and exclusions.
#' @noRd
get_relationship_coverage <- function(inspection_log) {
  sources <- inspection_log |>
    filter(.data$row_kind == "source", .data$context != "unsupported") |>
    summarise(
      location_count = n(), available_count = sum(.data$available),
      expression_count = sum(.data$available & !get_is_missing(.data$expression)),
      reference_count = sum(.data$reference_count),
      .by = "metadata_column"
    ) |>
    mutate(
      .keep = "none",
      scope = "sources", metadata_column = .data$metadata_column,
      status = case_when(
        .data$available_count == 0 ~ "unavailable",
        .data$available_count < .data$location_count ~ "partially_available",
        TRUE ~ "available"
      ),
      location_count = .data$location_count, reference_count = .data$reference_count,
      note = paste0(
        .data$available_count, " of ", .data$location_count,
        " locations available; ", .data$expression_count, " nonblank expressions inspected."
      ),
      examples = ""
    )
  notes <- inspection_log |>
    filter(.data$status %in% c("unresolved", "unclassified", "context_dependent"))
  project_notes <- if (nrow(notes) == 0) {
    sources[0, ]
  } else {
    tibble(
      scope = "project_notes", metadata_column = NA_character_, status = "interpretation_limits",
      location_count = n_distinct(notes$source_id), reference_count = sum(notes$row_kind == "reference"),
      note = paste(
        "Some project metadata could not be fully interpreted.",
        "Relationship reports may therefore omit references.",
        "Request Project Diagnostics to inspect those locations."
      ), examples = ""
    )
  }
  boundaries <- tibble(
    scope = "boundaries", metadata_column = NA_character_, status = "not_inspected",
    location_count = NA_integer_, reference_count = NA_integer_,
    note = paste(
      "Alerts, automated survey invitations, survey queues, report filters, data quality rules,",
      "external modules/scripts, and other project settings",
      "are outside the interpreted sources."
    ), examples = ""
  )
  bind_rows(sources, project_notes, boundaries) |>
    mutate(definitions_version = inspection_log$definitions_version[[1]], parser_version = inspection_log$parser_version[[1]])
}

#' @title Retain actionable interpretation issues for related metadata
#' @description Includes original evidence even when it is outside the reference chain.
#' @param diagnostics Project-wide interpretation issues.
#' @param report Selected field and related items.
#' @returns One row per related metadata location and interpretation status.
#' @noRd
get_relationship_issues <- function(diagnostics, report) {
  fields <- c(report$field$field_name, report$upstream$field_name, report$downstream$field_name)
  settings <- report$downstream$item_name[report$downstream$item_kind == "project_setting"]
  diagnostics |>
    filter(
      (.data$dependent_kind == "field" & .data$dependent_name %in% fields) |
        (.data$dependent_kind == "project_setting" & .data$dependent_name %in% settings) |
        map_lgl(.data$occurrence_ids, \(ids) any(ids %in% report$references$occurrence_id))
    )
}

#' @title Summarize project-wide interpretation issues
#' @description Retains issue evidence without blank, inspected, or unavailable source rows.
#' @param inspection_log Complete project inspection log.
#' @returns One row per metadata location and interpretation status.
#' @noRd
get_relationship_diagnostics <- function(inspection_log) {
  inspection_log |>
    filter(.data$status %in% c("unresolved", "unclassified", "context_dependent")) |>
    summarise(
      detail = paste(unique(if (any(.data$row_kind == "source")) {
        .data$detail[.data$row_kind == "source"]
      } else {
        .data$detail
      }), collapse = " "),
      occurrence_ids = list(.data$occurrence_id[!is.na(.data$occurrence_id)]),
      .by = c("source_id", "dependent_kind", "dependent_name", "metadata_column", "choice_code", "expression", "status")
    ) |>
    mutate(status_note = get_relationship_status_note(.data$status, .data$detail))
}
