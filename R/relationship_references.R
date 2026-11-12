#' @title Assemble metadata locations available for relationship extraction
#' @description Retains source expressions and distinguishes unavailable columns.
#' @param codebook A validated codebook with complete definitions.
#' @returns A tibble of metadata locations and their interpretation contexts.
#' @noRd
get_relationship_sources <- function(codebook) {
  fields <- codebook$definitions$fields
  locations <- c(
    branching_logic = "expression", select_choices_or_calculations = "calculation",
    field_annotation = "annotation", field_label = "text", field_note = "text",
    section_header = "text"
  )
  sources <- bind_rows(map(names(locations), \(column) {
    available <- column %in% codebook$definitions$source_columns$fields
    value <- if (available) as.character(fields[[column]]) else rep(NA_character_, nrow(fields))
    tibble(
      dependent_kind = "field", dependent_name = fields$field_name,
      metadata_column = column, choice_code = NA_character_, expression = value,
      context = ifelse(column == "select_choices_or_calculations" & fields$field_type != "calc",
        "choices", locations[[column]]
      ),
      available = available
    )
  })) |>
    filter(.data$context != "choices")

  choice_sources <- bind_rows(map(which(fields$field_type %in% c("radio", "dropdown", "checkbox")), \(i) {
    definition <- fields$select_choices_or_calculations[[i]]
    if (get_is_missing(definition)) {
      return(tibble())
    }
    parts <- strsplit(as.character(definition), "|", fixed = TRUE)[[1]]
    bind_rows(map(parts, \(part) {
      comma <- str_locate(part, ",")[1, "start"]
      if (is.na(comma)) {
        return(tibble())
      }
      tibble(
        dependent_kind = "field", dependent_name = fields$field_name[[i]],
        metadata_column = "choice_label", choice_code = trimws(substr(part, 1, comma - 1L)),
        expression = substr(part, comma + 1L, nchar(part)), context = "text", available = TRUE
      )
    }))
  }))
  project <- codebook$definitions$project
  project_sources <- bind_rows(map(names(project), \(column) {
    tibble(
      dependent_kind = "project_setting", dependent_name = column,
      metadata_column = column, choice_code = NA_character_,
      expression = if (nrow(project) == 0) NA_character_ else as.character(project[[column]][[1]]),
      context = if (column == "custom_record_label") "text" else "unsupported",
      available = column %in% codebook$definitions$source_columns$project
    )
  }))
  if (!"custom_record_label" %in% names(project)) {
    project_sources <- bind_rows(project_sources, tibble(
      dependent_kind = "project_setting", dependent_name = "custom_record_label",
      metadata_column = "custom_record_label", choice_code = NA_character_,
      expression = NA_character_, context = "text", available = FALSE
    ))
  }
  bind_rows(sources, choice_sources, project_sources) |>
    mutate(dictionary_order = match(.data$dependent_name, fields$field_name)) |>
    arrange(.data$dictionary_order) |>
    select(-"dictionary_order") |>
    mutate(source_id = row_number(), .before = 1)
}

#' @title Build typed relationship occurrences for the supplied project
#' @description Assigns report-local IDs in source and token order.
#' @param sources Metadata locations.
#' @param codebook A validated codebook.
#' @returns A typed occurrence table, including unresolved candidates.
#' @noRd
get_project_relationship_references <- function(sources, codebook) {
  prototype <- tibble(
    occurrence_id = integer(), source_id = integer(), source_field = character(),
    dependent_kind = character(), dependent_name = character(), metadata_column = character(),
    choice_code = character(), expression = character(), reference_text = character(),
    start = integer(), end = integer(), relationship = character(), action_tag = character(),
    field_matched = logical(), interpretation = character(),
    interpretation_reason = character(), event = character(), instance = character(),
    modifier = character(), reference_choice = character(), resolution = character(),
    reason = character(), event_context = character()
  )
  raw <- bind_rows(map(seq_len(nrow(sources)), \(i) get_relationship_source_references(sources[i, ])))
  if (nrow(raw) == 0) {
    return(prototype)
  }
  raw <- bind_rows(map(seq_len(nrow(raw)), \(i) get_relationship_display_pipes(raw[i, ], codebook)))
  bind_rows(map(seq_len(nrow(raw)), \(i) get_resolved_relationship(raw[i, ], codebook))) |>
    arrange(.data$source_id, .data$start) |>
    mutate(occurrence_id = row_number(), .before = 1) |>
    select(all_of(names(prototype)))
}

#' @title Locate reference tokens without evaluating source expressions
#' @description Expression contexts exclude quoted literals; display contexts
#'   include piping inside quoted text and basic field embedding.
#' @param text A single metadata expression.
#' @param context Either expression or text.
#' @returns Reference text with one-based inclusive character spans.
#' @noRd
get_relationship_tokens <- function(text, context = "expression") {
  references <- r"{\[[^\[\]\r\n]+\](?:\s*\[[^\[\]\r\n]+\])*|\{[A-Za-z][A-Za-z0-9_]*\}}"
  pattern <- if (context == "expression") {
    paste0(r"{'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"|}", references)
  } else {
    references
  }
  matches <- str_locate_all(text, pattern)[[1]]
  if (nrow(matches) == 0) {
    return(tibble(reference_text = character(), start = integer(), end = integer()))
  }
  tokens <- tibble(
    reference_text = str_sub(text, matches[, "start"], matches[, "end"]),
    start = matches[, "start"], end = matches[, "end"]
  )
  tokens |>
    filter(startsWith(.data$reference_text, "[") | startsWith(.data$reference_text, "{"))
}

#' @title Identify annotation argument regions
#' @description Tracks parentheses and quoting, including tags nested in IF branches.
#' @param text A complete field annotation.
#' @returns Annotation tags with their character spans and parsing status.
#' @noRd
get_relationship_annotation_regions <- function(text) {
  matches <- str_locate_all(text, "@[A-Za-z][A-Za-z0-9_-]*")[[1]]
  if (nrow(matches) == 0) {
    return(tibble(tag = character(), start = integer(), end = integer(), complete = logical(), branch_start = integer()))
  }
  regions <- bind_rows(map(seq_len(nrow(matches)), \(index) {
    start <- matches[index, "start"]
    tag_end <- matches[index, "end"]
    position <- tag_end + 1L
    while (position <= nchar(text) && str_detect(str_sub(text, position, position), "\\s")) position <- position + 1L
    opener <- substr(text, position, position)
    if (opener == "=") {
      position <- position + 1L
      while (position <= nchar(text) && str_detect(str_sub(text, position, position), "\\s")) position <- position + 1L
      opener <- substr(text, position, position)
    }
    quote <- ""
    depth <- 0L
    complete <- FALSE
    branch_start <- NA_integer_
    end <- nchar(text)
    if (opener %in% c("(", "'", '"')) {
      for (i in seq.int(position, nchar(text))) {
        char <- substr(text, i, i)
        if (i > position && substr(text, i - 1L, i - 1L) == "\\") next
        if (quote != "") {
          if (char == quote) {
            quote <- ""
            if (depth == 0) {
              end <- i
              complete <- TRUE
              break
            }
          }
        } else if (char %in% c("'", '"')) {
          quote <- char
        } else if (char == "," && depth == 1 && is.na(branch_start)) {
          branch_start <- i
        } else if (char == "(") {
          depth <- depth + 1L
        } else if (char == ")") {
          depth <- depth - 1L
          if (depth == 0) {
            end <- i
            complete <- TRUE
            break
          }
        }
      }
    } else {
      end <- if (index < nrow(matches)) matches[index + 1L, "start"] - 1L else nchar(text)
    }
    tibble(
      tag = toupper(substr(text, start, tag_end)), start = start, end = end,
      complete = complete, branch_start = branch_start
    )
  }))
  nested_tags <- map_lgl(seq_len(nrow(regions)), \(i) {
    parents <- regions[regions$start < regions$start[[i]] & regions$end >= regions$end[[i]], ]
    all(parents$tag == "@IF" & !is.na(parents$branch_start) & regions$start[[i]] > parents$branch_start)
  })
  regions[nested_tags, ]
}

#' @title Extract references from one metadata location
#' @description Applies calculation, display, or annotation-specific interpretation.
#' @param source One row from the metadata location table.
#' @returns Reference occurrences before field and context resolution.
#' @noRd
get_relationship_source_references <- function(source) {
  context <- source$context
  expression <- source$expression
  if (!source$available || get_is_missing(expression)) {
    return(tibble())
  }
  tokens <- get_relationship_tokens(expression, if (context %in% c("expression", "calculation")) "expression" else "text")
  if (nrow(tokens) == 0) {
    return(tibble())
  }
  relationship <- switch(context,
    expression = "visibility_condition",
    calculation = "calculation_input",
    text = "displayed_value",
    "unclassified"
  )
  tokens$action_tag <- NA_character_
  tokens$relationship <- relationship
  tokens$interpretation <- if (context == "unsupported") "unclassified" else "recognized"
  tokens$interpretation_reason <- if (context == "unsupported") "This project setting is not interpreted." else NA_character_
  if (context == "annotation") {
    regions <- get_relationship_annotation_regions(expression)
    tokens <- bind_rows(map(seq_len(nrow(tokens)), \(i) {
      token <- tokens[i, ]
      enclosing <- regions[regions$start <= token$start & regions$end >= token$end, ]
      region <- if (nrow(enclosing) == 0) NULL else enclosing[which.min(enclosing$end - enclosing$start), ]
      tag <- if (is.null(region)) "" else region$tag
      token$action_tag <- if (tag == "") NA_character_ else tag
      token$relationship <- switch(tag,
        "@DEFAULT" = "initial_value",
        "@CALCTEXT" = "calculation_input",
        "@CALCDATE" = "calculation_input",
        "@IF" = "annotation_condition",
        "unclassified"
      )
      recognized <- !is.null(region) && region$complete && token$relationship != "unclassified"
      if (recognized && tag != "@DEFAULT") {
        body <- substr(expression, region$start, region$end)
        unquoted <- get_relationship_tokens(body, "expression")
        recognized <- (token$start - region$start + 1L) %in% unquoted$start
      }
      if (!recognized) {
        token$interpretation <- "unclassified"
        token$interpretation_reason <- "Unsupported, incomplete, or ambiguous annotation argument."
      }
      token
    }))
  }
  tokens$relationship[startsWith(tokens$reference_text, "{") & context == "text"] <- "embedded_field"
  columns <- c("source_id", "dependent_kind", "dependent_name", "metadata_column", "choice_code", "expression")
  for (column in columns) tokens[[column]] <- source[[column]]
  tokens
}

#' @title Resolve field identity and event or instance qualifiers
#' @description Retains unresolved candidates without guessing their context.
#' @param token One reference occurrence.
#' @param codebook A validated codebook.
#' @returns One resolved occurrence with original evidence preserved.
#' @noRd
get_resolved_relationship <- function(token, codebook) {
  text <- token$reference_text
  embedding <- startsWith(text, "{")
  parts <- if (embedding) {
    substring(text, 2L, nchar(text) - 1L)
  } else {
    matches <- str_extract_all(text, "\\[[^\\]]+\\]")[[1]]
    substring(matches, 2L, nchar(matches) - 1L)
  }
  fields <- codebook$definitions$fields
  base <- str_remove(parts, "[:(].*$")
  candidates <- which(base %in% fields$field_name)
  # A bare unknown field is still a reference, rather than an omitted token.
  field_index <- if (length(candidates) == 1) candidates else if (length(parts) == 1) 1L else NA_integer_
  field <- if (is.na(field_index)) NA_character_ else base[[field_index]]
  field_token <- if (is.na(field_index)) "" else parts[[field_index]]
  event <- if (!is.na(field_index) && field_index > 1) parts[[field_index - 1L]] else NA_character_
  instance <- if (!is.na(field_index) && field_index < length(parts)) parts[[field_index + 1L]] else NA_character_
  modifier <- str_match(field_token, "^[^:]+:(.*)$")[1, 2]
  choice <- str_match(field_token, "^[^(]+\\(([^)]+)\\)")[1, 2]
  issues <- character()
  dynamic <- character()
  if (length(parts) > 3 || (!is.na(field_index) && field_index > 2)) {
    issues <- c(issues, "Reference qualifier chain is not interpreted.")
  }
  if (is.na(field) || !field %in% fields$field_name) {
    issues <- c(issues, "Field identity could not be resolved.")
  } else {
    if (embedding && token$dependent_kind == "field" &&
      fields$form_name[match(field, fields$field_name)] != fields$form_name[match(token$dependent_name, fields$field_name)]) {
      issues <- c(issues, "Field embedding requires fields on the same instrument.")
    }
    if (!is.na(choice) && !any(codebook$choices$field_name == field &
      codebook$choices$choice_value == choice & codebook$choices$choice_type == "checkbox")) {
      issues <- c(issues, "Checkbox choice is not defined for this field.")
    }
    if (!is.na(event)) {
      if (event %in% c("current-event-name", "previous-event-name", "next-event-name", "first-event-name", "last-event-name")) {
        dynamic <- c(dynamic, "Event is selected dynamically.")
      } else if (!event %in% codebook$events[["redcap_event_name"]]) {
        issues <- c(issues, "Unique event name is not defined.")
      } else if (nrow(codebook$event_instruments) == 0) {
        dynamic <- c(dynamic, "Event-instrument mapping is unavailable.")
      } else if (!any(codebook$event_instruments[["redcap_event_name"]] == event &
        codebook$event_instruments[["form_name"]] == fields$form_name[match(field, fields$field_name)])) {
        issues <- c(issues, "Field's instrument is not mapped to the explicit event.")
      }
    }
    if (!is.na(instance) && !str_detect(instance, "^[1-9][0-9]*$")) {
      if (instance %in% c("current-instance", "previous-instance", "next-instance", "first-instance", "last-instance")) {
        dynamic <- c(dynamic, "Instance is selected dynamically.")
      } else {
        issues <- c(issues, "Instance qualifier is not recognized.")
      }
    }
    if (!is.na(modifier) && !modifier %in% c("value", "label", "checked", "unchecked", "checked:value", "unchecked:value")) {
      issues <- c(issues, "Piping modifier is not interpreted.")
    }
  }
  status <- if (length(issues) > 0) "unresolved" else if (length(dynamic) > 0) "context_dependent" else "resolved"
  reason <- if (length(c(issues, dynamic)) == 0) NA_character_ else paste(c(issues, dynamic), collapse = " ")
  if (token$interpretation == "unclassified") {
    status <- "unclassified"
    reason <- token$interpretation_reason
  }
  token$field_matched <- !is.na(field) && field %in% fields$field_name
  token$source_field <- field
  token$event <- event
  token$instance <- instance
  token$modifier <- modifier
  token$reference_choice <- choice
  token$resolution <- status
  token$reason <- reason
  token$event_context <- if (is.na(event)) "current" else "explicit_or_dynamic"
  token
}

#' @title Separate adjacent field pipes in display text
#' @description Distinguishes consecutive known fields from event qualifiers.
#' @param token One extracted occurrence.
#' @param codebook A validated codebook.
#' @returns One or more occurrences with exact original character spans.
#' @noRd
get_relationship_display_pipes <- function(token, codebook) {
  if (!token$relationship %in% c("displayed_value", "initial_value")) {
    return(token)
  }
  matches <- str_locate_all(token$reference_text, "\\[[^\\]]+\\]")[[1]]
  if (nrow(matches) < 2) {
    return(token)
  }
  parts <- str_sub(token$reference_text, matches[, "start"], matches[, "end"])
  fields <- str_remove(str_sub(parts, 2L, -2L), "[:(].*$")
  if (!all(fields %in% codebook$definitions$fields$field_name) ||
    fields[[1]] %in% codebook$events[["redcap_event_name"]]) {
    return(token)
  }
  out <- token[rep(1L, length(parts)), ]
  out$reference_text <- parts
  out$start <- token$start + matches[, "start"] - 1L
  out$end <- out$start + nchar(parts) - 1L
  out
}
