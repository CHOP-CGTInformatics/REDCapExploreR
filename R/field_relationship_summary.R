#' @title Summarize how a REDCap field is related to project metadata
#' @description Explains the selected field's inputs, references, and direct or
#'   indirect connections using a codebook's complete definitions.
#' @param field One exact dictionary field name, including checkbox parent names.
#' @param codebook A codebook returned by [build_codebook()].
#' @param include_diagnostics Include the full project-wide source and reference
#'   diagnostics as an additional `diagnostics` tibble. Default is `FALSE`.
#' @details
#' A codebook supplies field definitions, expressions, and event/repeat context
#' needed to interpret a field name. Build it once to explore several fields
#' from the same metadata without repeated API calls. Record data is not needed.
#'
#' Recognized sources include branching logic, calculations, supported field
#' annotations, piping, embedding, and the custom record label. Unresolved or
#' unsupported constructs are retained as interpretation issues rather than guessed. The
#' report describes relationships; it does not predict changes to stored values.
#' Older codebooks without complete definitions must be rebuilt.
#' @returns A `redcap_field_relationship_summary` list with seven tibbles:
#'   `field` (complete selected definition and context), `upstream` (what the
#'   field uses), `references` (related occurrences with original expressions and
#'   report-local IDs), `downstream` (where the field is used), `summary`
#'   (explanations grouped by metadata location and expression), `issues`
#'   (related interpretation issues with original evidence), and `coverage`
#'   (project-wide source counts and scope boundaries). With `include_diagnostics = TRUE`,
#'   the additional `diagnostics` element retains all metadata locations and issues.
#'   `relationship` contains machine-readable connection types; `resolution`
#'   describes qualifier or syntax interpretation, not data quality.
#'   `field_matched` records whether the referenced field name exists. Known
#'   references remain linked when qualifiers need interpretation; unsupported
#'   annotation candidates do not create traversable links. `status_note` explains
#'   non-resolved references and is blank for resolved references.
#'   Upstream and downstream rows include minimum depth and a representative
#'   shortest path of occurrence IDs. All alternative related edges remain in
#'   `references`. Reference chains label each step but do not establish that
#'   values or behavior will change, or that event/instance contexts connect.
#'   Matched references do not validate REDCap configuration or establish
#'   foreign-key relationships or referential integrity.
#' @examples
#' relationships <- field_relationship_summary("weight_kg", mock_codebook)
#' relationships
#' relationships$summary
#' @export
field_relationship_summary <- function(field, codebook, include_diagnostics = FALSE) {
  get_validate_relationship_input(field, codebook, include_diagnostics)
  sources <- get_relationship_sources(codebook)
  references <- get_project_relationship_references(sources, codebook)
  edges <- references |>
    filter(.data$field_matched, .data$interpretation == "recognized") |>
    mutate(
      from = paste0("field:", .data$source_field),
      to = paste(.data$dependent_kind, .data$dependent_name, sep = ":")
    )
  upstream <- get_relationship_traversal(field, edges, "upstream")
  downstream <- get_relationship_traversal(field, edges, "downstream")
  seed <- paste0("field:", field)
  upstream_nodes <- c(seed, upstream$node)
  downstream_nodes <- c(seed, downstream$node)
  upstream_edges <- paste0("field:", references$source_field) %in% upstream_nodes &
    paste(references$dependent_kind, references$dependent_name, sep = ":") %in% upstream_nodes
  downstream_edges <- paste0("field:", references$source_field) %in% downstream_nodes &
    paste(references$dependent_kind, references$dependent_name, sep = ":") %in% downstream_nodes
  direct <- references$source_field %in% field |
    (references$dependent_kind == "field" & references$dependent_name == field)
  related <- references[upstream_edges | downstream_edges | direct, ]
  related$traversal_role <- case_when(
    related$source_field == field & related$dependent_kind == "field" & related$dependent_name == field ~ "self",
    upstream_edges[upstream_edges | downstream_edges | direct] &
      downstream_edges[upstream_edges | downstream_edges | direct] ~ "both",
    related$dependent_kind == "field" & related$dependent_name == field ~ "upstream",
    related$source_field == field ~ "downstream",
    upstream_edges[upstream_edges | downstream_edges | direct] ~ "upstream",
    TRUE ~ "downstream"
  )
  related$status_note <- get_relationship_status_note(related$resolution, related$reason)
  related$direct <- related$source_field %in% field |
    (related$dependent_kind == "field" & related$dependent_name == field)
  report <- list(
    field = get_relationship_definitions(codebook, field),
    upstream = get_relationship_items(codebook, upstream, references, "upstream"),
    references = related,
    downstream = get_relationship_items(codebook, downstream, references, "downstream"),
    summary = get_relationship_explanations(field, related, upstream, downstream)
  )
  diagnostics <- get_relationship_diagnostics(sources, references, codebook)
  report$issues <- get_relationship_issues(diagnostics, report)
  report$coverage <- get_relationship_coverage(diagnostics, report)
  if (include_diagnostics) report$diagnostics <- diagnostics
  class(report) <- c("redcap_field_relationship_summary", "list")
  report
}

#' @title Validate a field relationship request
#' @description Checks the complete definition schema at the public boundary.
#' @param field A requested field name.
#' @param codebook A codebook object.
#' @param include_diagnostics Whether detailed diagnostics were requested.
#' @returns Nothing; invalid inputs raise an error.
#' @noRd
get_validate_relationship_input <- function(field, codebook, include_diagnostics) {
  if (!is.logical(include_diagnostics) || length(include_diagnostics) != 1 || is.na(include_diagnostics)) {
    cli_abort("{.arg include_diagnostics} must be TRUE or FALSE.")
  }
  if (!is.character(field) || length(field) != 1 || is.na(field) || trimws(field) == "") {
    cli_abort("{.arg field} must be one nonblank dictionary field name.")
  }
  get_validate_codebook(codebook)
  definitions <- codebook$definitions
  required <- c("field_name", "form_name", "field_type", "branching_logic", "select_choices_or_calculations")
  if (!is.list(definitions) || !identical(definitions$schema_version, 1L) ||
    !is.data.frame(definitions$fields) || !is.data.frame(definitions$project) ||
    !is.list(definitions$source_columns) ||
    !is.character(definitions$source_columns$fields) ||
    !is.character(definitions$source_columns$project) ||
    !all(required %in% names(definitions$fields))) {
    cli_abort("Rebuild {.arg codebook} with {.fun build_codebook} to include complete definitions (schema 1).")
  }
  names <- definitions$fields$field_name
  if (!is.character(names) || any(get_is_missing(names)) || anyDuplicated(names)) {
    cli_abort("Codebook definitions must have unique, nonblank field names.")
  }
  if (!field %in% names) {
    cli_abort("{.field {field}} is not a dictionary field. Use an exact field name, including checkbox parent names.")
  }
  invisible(NULL)
}

#' @export
print.redcap_field_relationship_summary <- function(x, ...) {
  cat("<REDCap field relationships>\n")
  cat("Field: ", x$field$field_name, " (", x$field$form_name, ")\n", sep = "")
  cat("Upstream: ", nrow(x$upstream), if (nrow(x$upstream) == 1) " item\n" else " items\n", sep = "")
  direct <- sum(x$downstream$depth == 1)
  indirect <- sum(x$downstream$depth > 1)
  cat("Downstream: ", direct, " direct ", if (direct == 1) "item" else "items",
    ", ", indirect, " indirect ", if (indirect == 1) "item\n" else "items\n",
    sep = ""
  )
  if (nrow(x$references) == 0) {
    cat("No references to ", x$field$field_name, " were found in the inspected metadata.\n", sep = "")
  } else {
    cat("Reference occurrences across returned chains: ", nrow(x$references), "\n", sep = "")
    types <- x$references |>
      count(.data$relationship)
    cat(paste(get_relationship_type_label(types$relationship), types$n, sep = ": ", collapse = "; "), "\n", sep = "")
  }
  cat("Interpretation issues: ", n_distinct(x$issues$source_id), " related metadata locations (see issues).\n", sep = "")
  cat("Coverage: project-wide metadata inspection; see coverage for sources and exclusions.\n")
  cat("Indirect connections are reference chains, not predictions of value or behavior changes.\n")
  if (any(x$coverage$scope == "project_notes")) {
    cat("Other interpretation limits elsewhere in the project are summarized in coverage.\n")
  }
  invisible(x)
}

#' @title Traverse field references in one direction
#' @description Finds minimum distances and deterministic shortest evidence paths.
#' @param field The selected field.
#' @param edges Recognized relationship occurrences with from/to node keys.
#' @param direction Either upstream or downstream.
#' @returns One row per reachable item, excluding the selected field.
#' @noRd
get_relationship_traversal <- function(field, edges, direction) {
  start <- paste0("field:", field)
  seen <- tibble(node = start, depth = 0L, path = list(integer()), field_path = list(field))
  frontier <- seen
  from <- if (direction == "downstream") "from" else "to"
  to <- if (direction == "downstream") "to" else "from"
  while (nrow(frontier) > 0) {
    next_rows <- bind_rows(map(seq_len(nrow(frontier)), \(i) {
      matches <- which(edges[[from]] == frontier$node[[i]])
      if (length(matches) == 0) {
        return(seen[0, ])
      }
      tibble(
        node = edges[[to]][matches], depth = frontier$depth[[i]] + 1L,
        path = map(matches, \(j) c(frontier$path[[i]], edges$occurrence_id[[j]])),
        field_path = map(matches, \(j) c(frontier$field_path[[i]], sub("^[^:]+:", "", edges[[to]][[j]])))
      )
    }))
    frontier <- next_rows |>
      filter(!.data$node %in% seen$node) |>
      distinct(.data$node, .keep_all = TRUE)
    seen <- bind_rows(seen, frontier)
  }
  seen |> filter(.data$node != start)
}

#' @title Attach complete field definitions to related items
#' @description Keeps project settings distinct from dictionary fields.
#' @param codebook A validated codebook.
#' @param traversal Reachable items and paths.
#' @param references Project reference occurrences.
#' @param direction Upstream or downstream.
#' @returns Related items with their available definitions and display context.
#' @noRd
get_relationship_items <- function(codebook, traversal, references, direction) {
  kind <- sub(":.*$", "", traversal$node)
  name <- sub("^[^:]+:", "", traversal$node)
  definition <- get_relationship_definitions(codebook, ifelse(kind == "field", name, NA_character_))
  out <- tibble(
    item_kind = kind, item_name = name, depth = traversal$depth,
    connection = if_else(traversal$depth == 1, "direct", "indirect"),
    path = traversal$path, field_path = traversal$field_path
  )
  out$relationship_types <- map_chr(traversal$path, \(path) {
    rows <- references[match(path, references$occurrence_id), ]
    paste(unique(get_relationship_type_label(rows$relationship, rows$action_tag)), collapse = "; ")
  })
  out$status_note <- map_chr(traversal$path, \(path) {
    notes <- get_relationship_status_note(
      references$resolution[match(path, references$occurrence_id)],
      references$reason[match(path, references$occurrence_id)]
    )
    paste(unique(notes[notes != ""]), collapse = " ")
  })
  out$reference_chain <- map2_chr(
    traversal$field_path, traversal$path,
    \(fields, path) get_relationship_chain(fields, path, references, direction)
  )
  for (column in names(definition)) out[[column]] <- definition[[column]]
  out
}

#' @title Retrieve definitions with event and instrument display context
#' @param codebook A validated codebook.
#' @param fields Field names; NA represents a non-field item.
#' @description Preserves full expressions and annotations in dictionary metadata.
#' @returns A tibble with one row for each requested field.
#' @noRd
get_relationship_definitions <- function(codebook, fields) {
  definitions <- codebook$definitions$fields
  out <- definitions[match(fields, definitions$field_name), , drop = FALSE]
  context <- codebook$fields |>
    select("field_name", any_of(c("form_label", "event_names", "event_count", "repeating_status")))
  left_join(out, context, by = "field_name", relationship = "many-to-one")
}

#' @title Explain the meaning of related reference occurrences
#' @description Groups repeated occurrences without predicting changes to data.
#' @param field The selected field.
#' @param references Related occurrences with traversal roles.
#' @param upstream Upstream traversal paths.
#' @param downstream Downstream traversal paths.
#' @returns Relationship explanations and their supporting occurrence IDs.
#' @noRd
get_relationship_explanations <- function(field, references, upstream, downstream) {
  out <- references |>
    summarise(
      occurrence_ids = list(.data$occurrence_id),
      occurrence_count = n(),
      reason = paste(unique(.data$reason[!get_is_missing(.data$reason)]), collapse = " "),
      direct = any(.data$direct),
      .by = c(
        "source_field", "dependent_kind", "dependent_name", "source_id", "metadata_column",
        "choice_code", "expression", "relationship", "action_tag", "field_matched", "resolution",
        "traversal_role", "event", "instance", "modifier", "reference_choice"
      )
    ) |>
    mutate(
      verb = case_when(
        .data$resolution == "unclassified" ~ "contains an unclassified candidate reference to ",
        .data$resolution == "unresolved" ~ "contains an unresolved reference to ",
        .data$relationship == "calculation_input" ~ "uses ",
        .data$relationship %in% c("visibility_condition", "initial_value", "annotation_condition") ~ "references ",
        .data$relationship == "embedded_field" ~ "embeds ",
        TRUE ~ "pipes data or labels from "
      ),
      purpose = case_when(
        .data$resolution %in% c("unresolved", "unclassified") ~ "",
        .data$relationship == "calculation_input" ~ " as a calculation input",
        .data$relationship == "visibility_condition" ~ " in its branching logic",
        .data$relationship == "initial_value" ~ " when assigning its default value",
        .data$relationship == "annotation_condition" ~ " in its @IF action-tag condition",
        TRUE ~ ""
      ),
      explanation = paste0(
        .data$dependent_name, " ", .data$verb,
        if_else(is.na(.data$source_field), "an unidentified field", .data$source_field), .data$purpose, ".",
        if_else(.data$direct, "", paste0(" Connected to ", field, " through a reference chain.")),
        if_else(.data$resolution == "context_dependent", " Event or instance context needs interpretation.", "")
      )
    ) |>
    select(-c("verb", "purpose"))
  paths <- map(seq_len(nrow(out)), \(i) {
    incoming <- out$traversal_role[[i]] == "upstream" ||
      (out$dependent_kind[[i]] == "field" && out$dependent_name[[i]] == field)
    traversal <- if (incoming) upstream else downstream
    key <- if (incoming) {
      paste(out$dependent_kind[[i]], out$dependent_name[[i]], sep = ":")
    } else {
      paste0("field:", out$source_field[[i]])
    }
    index <- match(key, traversal$node)
    prefix <- if (is.na(index)) integer() else traversal$path[[index]]
    names <- if (is.na(index)) field else traversal$field_path[[index]]
    last <- if (incoming) out$source_field[[i]] else out$dependent_name[[i]]
    list(path = c(prefix, out$occurrence_ids[[i]][1]), field_path = c(names, last))
  })
  out$status_note <- get_relationship_status_note(out$resolution, out$reason)
  out$path <- map(paths, "path")
  out$field_path <- map(paths, "field_path")
  out$reference_chain <- map_chr(seq_len(nrow(out)), \(i) {
    direction <- if (out$traversal_role[[i]] == "upstream" ||
      (out$dependent_kind[[i]] == "field" && out$dependent_name[[i]] == field)) {
      "upstream"
    } else {
      "downstream"
    }
    get_relationship_chain(out$field_path[[i]], out$path[[i]], references, direction)
  })
  out
}

#' @title Describe reference chains without implying value propagation
#' @description Labels each step and preserves the direction of the underlying reference.
#' @param fields Ordered field or setting names along the traversal.
#' @param path Occurrence IDs for the steps.
#' @param references Reference occurrences providing step types.
#' @param direction Upstream or downstream traversal.
#' @returns A readable reference chain.
#' @noRd
get_relationship_chain <- function(fields, path, references, direction) {
  rows <- references[match(path, references$occurrence_id), ]
  labels <- get_relationship_type_label(rows$relationship, rows$action_tag)
  steps <- if (direction == "upstream") {
    paste0(" <-[", labels, "]- ", fields[-1])
  } else {
    paste0(" -[", labels, "]-> ", fields[-1])
  }
  paste0(fields[[1]], paste(steps, collapse = ""))
}

#' @title Label relationship types for readers
#' @description Keeps machine-readable codes separate from presentation labels.
#' @param relationship Typed relationship codes.
#' @param action_tag Specific action tag, when present.
#' @returns Human-readable labels.
#' @noRd
get_relationship_type_label <- function(relationship, action_tag = NA_character_) {
  labels <- c(
    calculation_input = "Calculation", visibility_condition = "Branching logic",
    displayed_value = "Piping", initial_value = "Default value",
    annotation_condition = "Action-tag condition", embedded_field = "Field embedding",
    unclassified = "Unknown relationship type"
  )
  action_tag <- rep_len(action_tag, length(relationship))
  label <- unname(labels[relationship])
  if_else(is.na(action_tag), label, paste0(label, " (", action_tag, ")"))
}

#' @title Explain reference interpretation when it needs attention
#' @description Resolved references have no interpretation note; other statuses
#'   retain the actual extraction or context reason.
#' @param resolution Reference interpretation status.
#' @param reason Source or qualifier interpretation details.
#' @returns Character notes, blank for resolved references.
#' @noRd
get_relationship_status_note <- function(resolution, reason) {
  label <- case_when(
    resolution == "context_dependent" ~ "Context dependent",
    resolution == "unresolved" ~ "Unresolved reference",
    resolution == "unclassified" ~ "Unclassified reference",
    TRUE ~ ""
  )
  if_else(resolution == "resolved", "", paste0(label, ifelse(get_is_missing(reason), "", paste0(": ", reason))))
}
