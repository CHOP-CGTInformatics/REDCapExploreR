#' @title Build a REDCap codebook
#'
#' @description
#' `build_codebook()` pulls REDCap project metadata through the API and returns
#' a structured, tidy codebook intended to resemble the information users see in
#' the REDCap UI codebook while remaining useful for downstream R workflows.
#'
#' @param redcap_uri REDCap API URI.
#' @param token REDCap API token.
#'
#' @returns A `redcap_codebook` list with these elements:
#' - `fields`: one row per REDCap field with display-oriented metadata,
#'   parsed choice summaries, validation, branching logic, event applicability,
#'   and repeating status when available.
#' - `choices`: one row per parsed choice option.
#' - `forms`: one row per instrument/form.
#' - `events`: one row per REDCap event when the project is longitudinal.
#' - `event_instruments`: one row per event/form mapping when available.
#' - `repeating`: repeating instrument/event configuration when available.
#' - `project`: project-level summary metadata.
#'
#' @examples
#' \dontrun{
#' codebook <- build_codebook(
#'   redcap_uri = Sys.getenv("REDCAP_URI"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#'
#' codebook$fields
#' codebook$choices
#' }
#'
#' @export
build_codebook <- function(redcap_uri, token) {
  if (missing(redcap_uri)) {
    cli_abort("{.arg redcap_uri} and {.arg token} are required.")
  }
  get_validate_api_credentials(redcap_uri, token)

  project <- get_codebook_project(
    pull_redcap_codebook(
      redcap_uri = redcap_uri,
      token = token
    )
  )

  get_codebook(project)
}

get_codebook <- function(project) {
  codebook <- list(
    fields = get_codebook_fields(project),
    choices = get_codebook_choices(project),
    forms = get_codebook_forms(project),
    events = project$events,
    event_instruments = project$event_instruments,
    repeating = project$repeating_instruments,
    project = get_codebook_project_summary(project)
  )

  class(codebook) <- c("redcap_codebook", class(codebook))
  codebook
}

#' @title View a REDCap codebook as interactive HTML tables
#'
#' @description
#' `view_codebook()` turns a `redcap_codebook` object from [build_codebook()]
#' into a static HTML viewer with tab-style section navigation and one
#' interactive table per codebook section. The returned object prints in the
#' RStudio/Posit Viewer and can be saved as an HTML file with
#' [htmltools::save_html()]. Event, event-instrument, and repeating-structure
#' sections are omitted when those API tables are empty.
#'
#' @param codebook A `redcap_codebook` object returned by [build_codebook()].
#' @param page_length Number of rows to show per interactive table page.
#'
#' @returns An `htmltools` browsable HTML object containing interactive `DT`
#'   tables.
#'
#' @examples
#' \dontrun{
#' codebook <- build_codebook(
#'   redcap_uri = Sys.getenv("REDCAP_URI"),
#'   token = Sys.getenv("REDCAP_TOKEN")
#' )
#'
#' viewer <- view_codebook(codebook)
#' htmltools::save_html(viewer, "codebook.html")
#' }
#'
#' @export
view_codebook <- function(codebook, page_length = 25) {
  get_validate_codebook(codebook)
  get_validate_page_length(page_length)

  browsable(
    tagList(
      get_codebook_viewer_deps(),
      tags$main(
        class = "redcap-codebook-viewer",
        tags$header(
          id = "top",
          class = "redcap-codebook-header",
          tags$h1(codebook$project$project_title[[1]]),
          tags$p(
            class = "redcap-codebook-subtitle",
            "REDCap Codebook"
          ),
          tags$p(
            class = "redcap-codebook-intro",
            paste(
              "Review this project's fields, forms, choices, events, and",
              "repeat settings using the section tabs below. Each table can",
              "be searched, sorted, paged, and scrolled horizontally for",
              "wide metadata."
            )
          ),
          get_codebook_viewer_summary(codebook)
        ),
        get_codebook_viewer_navigation(codebook),
        get_codebook_viewer_sections(codebook, page_length)
      )
    )
  )
}

pull_redcap_codebook <- function(redcap_uri, token) {
  get_validate_api_credentials(redcap_uri, token)

  metadata_result <- redcap_metadata_read(
    redcap_uri = redcap_uri,
    token = token,
    verbose = FALSE
  )

  list(
    metadata = get_redcapr_data(metadata_result, "metadata"),
    events = get_optional_redcapr_data(redcap_event_read, redcap_uri, token),
    event_instruments = get_optional_redcapr_data(
      redcap_event_instruments,
      redcap_uri,
      token
    ),
    instruments = get_optional_redcapr_data(
      redcap_instruments,
      redcap_uri,
      token
    ),
    repeating_instruments = get_optional_redcapr_data(
      redcap_instrument_repeating,
      redcap_uri,
      token
    ),
    arms = get_optional_redcapr_data(redcap_arm_export, redcap_uri, token),
    project_info = get_optional_redcapr_data(
      redcap_project_info_read,
      redcap_uri,
      token
    )
  )
}

#' @export
print.redcap_codebook <- function(x, ...) {
  project <- x$project

  cat("<REDCap codebook>\n")
  cat("Project: ", project$project_title, "\n", sep = "")
  cat("Forms: ", project$form_count, "\n", sep = "")
  cat("Fields: ", project$field_count, "\n", sep = "")
  cat("Events: ", project$event_count, "\n", sep = "")
  cat("Repeating enabled: ", project$repeating_enabled, "\n", sep = "")
  invisible(x)
}

get_codebook_project <- function(project) {
  if (
    !is.list(project) ||
      is.data.frame(project) ||
      !"metadata" %in% names(project)
  ) {
    cli_abort(
      "The REDCap codebook API pull must return a {.field metadata} table."
    )
  }

  list(
    metadata = get_standard_metadata(project$metadata),
    events = get_standard_events(project$events),
    event_instruments = get_standard_event_instruments(
      project$event_instruments
    ),
    instruments = get_codebook_instruments(project$instruments),
    repeating_instruments = get_codebook_repeating(
      project$repeating_instruments
    ),
    arms = get_codebook_arms(project$arms),
    project_info = get_codebook_project_info(project$project_info),
    source = "api"
  )
}

get_codebook_fields <- function(project) {
  metadata <- project$metadata |>
    mutate(field_order = row_number())

  forms <- get_codebook_forms(project)
  choices <- get_codebook_choices(project)
  event_summary <- get_codebook_event_summary(project)
  repeating_summary <- get_codebook_repeating_summary(project)

  metadata |>
    left_join(
      forms |> select("form_name", "form_label", "form_order"),
      by = "form_name"
    ) |>
    left_join(
      choices |> count(.data$field_name, name = "choice_count"),
      by = "field_name"
    ) |>
    left_join(get_codebook_choice_labels(choices), by = "field_name") |>
    left_join(event_summary, by = "form_name") |>
    left_join(repeating_summary, by = "form_name") |>
    mutate(
      choice_count = if_else(is.na(.data$choice_count), 0L, .data$choice_count),
      choices = if_else(is.na(.data$choices), NA_character_, .data$choices),
      validation = get_codebook_validation_label(
        .data$text_validation_type_or_show_slider_number,
        .data$text_validation_min,
        .data$text_validation_max
      ),
      identifier = tolower(as.character(.data$identifier)) %in%
        c("y", "yes", "1", "true"),
      descriptive_field = tolower(as.character(.data$field_type)) %in%
        c("descriptive", "descriptive_text"),
      event_count = if_else(is.na(.data$event_count), 0L, .data$event_count),
      event_names = if_else(
        is.na(.data$event_names),
        NA_character_,
        .data$event_names
      ),
      repeating_status = if_else(
        is.na(.data$repeating_status),
        "Not repeating",
        .data$repeating_status
      )
    ) |>
    transmute(
      .data$field_order,
      .data$form_order,
      .data$form_name,
      .data$form_label,
      .data$field_name,
      .data$field_label,
      .data$field_type,
      .data$descriptive_field,
      .data$required_field,
      .data$identifier,
      .data$choice_count,
      .data$choices,
      .data$validation,
      branching_logic = as.character(.data$branching_logic),
      .data$event_count,
      .data$event_names,
      .data$repeating_status,
      field_note = if ("field_note" %in% names(metadata)) {
        as.character(.data$field_note)
      } else {
        NA_character_
      },
      matrix_group_name = if ("matrix_group_name" %in% names(metadata)) {
        as.character(.data$matrix_group_name)
      } else {
        NA_character_
      }
    )
}

get_codebook_choices <- function(project) {
  metadata <- project$metadata |>
    mutate(field_order = row_number()) |>
    filter(!get_is_missing(.data$select_choices_or_calculations))

  if (nrow(metadata) == 0) {
    return(tibble(
      field_order = integer(),
      field_name = character(),
      form_name = character(),
      choice_order = integer(),
      choice_value = character(),
      choice_label = character()
    ))
  }

  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    choices <- strsplit(
      metadata$select_choices_or_calculations[[index]],
      "\\|"
    )[[1]]

    bind_rows(map(seq_along(choices), \(choice_index) {
      parts <- strsplit(choices[[choice_index]], ",", fixed = TRUE)[[1]]
      tibble(
        field_order = metadata$field_order[[index]],
        field_name = metadata$field_name[[index]],
        form_name = metadata$form_name[[index]],
        choice_order = choice_index,
        choice_value = str_trim(parts[[1]]),
        choice_label = str_trim(paste(parts[-1], collapse = ","))
      )
    }))
  }))
}

get_codebook_forms <- function(project) {
  forms <- project$metadata |>
    distinct(.data$form_name) |>
    mutate(form_order = row_number())

  if (nrow(project$instruments) > 0) {
    forms <- forms |>
      left_join(project$instruments, by = "form_name")
  } else {
    forms$form_label <- NA_character_
  }

  forms |>
    mutate(
      form_label = if_else(
        get_is_missing(.data$form_label),
        .data$form_name,
        .data$form_label
      )
    ) |>
    left_join(
      project$metadata |>
        count(.data$form_name, name = "field_count"),
      by = "form_name"
    ) |>
    select("form_order", "form_name", "form_label", "field_count")
}

get_codebook_project_summary <- function(project) {
  project_info <- project$project_info
  project_title <- if (
    "project_title" %in% names(project_info) && nrow(project_info) > 0
  ) {
    as.character(project_info$project_title[[1]])
  } else {
    "Unknown REDCap project"
  }

  tibble(
    project_title = project_title,
    project_id = get_project_info_value(project_info, "project_id"),
    field_count = n_distinct(project$metadata$field_name),
    form_count = n_distinct(project$metadata$form_name),
    event_count = get_codebook_event_count(project),
    repeating_enabled = get_codebook_repeating_enabled(project),
    creation_time = get_project_info_value(
      project_info,
      "creation_time"
    ),
    production_time = get_project_info_value(
      project_info,
      "production_time"
    ),
    in_production = get_project_info_value(
      project_info,
      "in_production"
    ),
    project_language = get_project_info_value(
      project_info,
      "project_language"
    ),
    purpose = get_project_info_value(project_info, "purpose"),
    purpose_other = get_project_info_value(
      project_info,
      "purpose_other"
    ),
    project_notes = get_project_info_value(
      project_info,
      "project_notes"
    ),
    custom_record_label = get_project_info_value(
      project_info,
      "custom_record_label"
    ),
    secondary_unique_field = get_project_info_value(
      project_info,
      "secondary_unique_field"
    ),
    is_longitudinal = get_project_info_value(
      project_info,
      "is_longitudinal"
    ),
    has_repeating_instruments_or_events = get_project_info_value(
      project_info,
      "has_repeating_instruments_or_events"
    ),
    surveys_enabled = get_project_info_value(
      project_info,
      "surveys_enabled"
    ),
    scheduling_enabled = get_project_info_value(
      project_info,
      "scheduling_enabled"
    ),
    record_autonumbering_enabled = get_project_info_value(
      project_info,
      "record_autonumbering_enabled"
    ),
    randomization_enabled = get_project_info_value(
      project_info,
      "randomization_enabled"
    ),
    ddp_enabled = get_project_info_value(project_info, "ddp_enabled"),
    project_irb_number = get_project_info_value(
      project_info,
      "project_irb_number"
    ),
    project_grant_number = get_project_info_value(
      project_info,
      "project_grant_number"
    ),
    project_pi_firstname = get_project_info_value(
      project_info,
      "project_pi_firstname"
    ),
    project_pi_lastname = get_project_info_value(
      project_info,
      "project_pi_lastname"
    ),
    project_pi_email = get_project_info_value(
      project_info,
      "project_pi_email"
    ),
    missing_data_codes = get_project_info_value(
      project_info,
      "missing_data_codes"
    ),
    external_modules = get_project_info_value(
      project_info,
      "external_modules"
    )
  )
}

get_project_info_value <- function(project_info, field_name) {
  if (!field_name %in% names(project_info) || nrow(project_info) == 0) {
    return(NA)
  }

  project_info[[field_name]][[1]]
}

get_codebook_instruments <- function(instruments) {
  instruments <- get_optional_api_table(instruments)
  if (nrow(instruments) == 0 && ncol(instruments) == 0) {
    return(tibble(form_name = character(), form_label = character()))
  }

  names(instruments) <- get_clean_names(names(instruments))

  form_column <- intersect(
    c("instrument_name", "form_name", "instrument"),
    names(instruments)
  )
  label_column <- intersect(
    c("instrument_label", "form_label", "label"),
    names(instruments)
  )

  if (length(form_column) == 0) {
    return(tibble(form_name = character(), form_label = character()))
  }

  out <- instruments |>
    transmute(form_name = as.character(.data[[form_column[[1]]]]))

  out$form_label <- if (length(label_column) == 0) {
    NA_character_
  } else {
    as.character(instruments[[label_column[[1]]]])
  }

  out |>
    distinct(.data$form_name, .keep_all = TRUE)
}

get_codebook_repeating <- function(repeating_instruments) {
  repeating_instruments <- get_optional_api_table(repeating_instruments)
  if (nrow(repeating_instruments) == 0 && ncol(repeating_instruments) == 0) {
    return(tibble())
  }

  names(repeating_instruments) <- get_clean_names(names(repeating_instruments))

  if (
    !"form_name" %in% names(repeating_instruments) &&
      "instrument_name" %in% names(repeating_instruments)
  ) {
    repeating_instruments <- repeating_instruments |>
      mutate(form_name = as.character(.data$instrument_name))
  }

  if (
    !"redcap_event_name" %in% names(repeating_instruments) &&
      "unique_event_name" %in% names(repeating_instruments)
  ) {
    repeating_instruments <- repeating_instruments |>
      mutate(redcap_event_name = as.character(.data$unique_event_name))
  }

  repeating_instruments
}

get_codebook_arms <- function(arms) {
  arms <- get_optional_api_table(arms)
  if (nrow(arms) == 0 && ncol(arms) == 0) {
    return(arms)
  }

  names(arms) <- get_clean_names(names(arms))
  arms
}

get_codebook_project_info <- function(project_info) {
  project_info <- get_optional_api_table(project_info)
  if (nrow(project_info) == 0 && ncol(project_info) == 0) {
    return(tibble(project_title = character()))
  }

  names(project_info) <- get_clean_names(names(project_info))
  project_info
}

get_codebook_choice_labels <- function(choices) {
  if (nrow(choices) == 0) {
    return(tibble(field_name = character(), choices = character()))
  }

  choices |>
    group_by(.data$field_name) |>
    summarise(
      choices = paste(
        paste(.data$choice_value, .data$choice_label, sep = " = "),
        collapse = "\n"
      ),
      .groups = "drop"
    )
}

get_codebook_event_summary <- function(project) {
  event_instruments <- project$event_instruments
  if (
    nrow(event_instruments) == 0 ||
      !all(c("form_name", "redcap_event_name") %in% names(event_instruments))
  ) {
    return(tibble(
      form_name = character(),
      event_count = integer(),
      event_names = character()
    ))
  }

  event_instruments |>
    group_by(.data$form_name) |>
    summarise(
      event_count = n_distinct(.data$redcap_event_name),
      event_names = paste(unique(.data$redcap_event_name), collapse = ", "),
      .groups = "drop"
    )
}

get_codebook_repeating_summary <- function(project) {
  repeating <- project$repeating_instruments
  if (nrow(repeating) == 0 || !"form_name" %in% names(repeating)) {
    return(tibble(form_name = character(), repeating_status = character()))
  }

  repeating |>
    filter(!get_is_missing(.data$form_name)) |>
    distinct(.data$form_name) |>
    mutate(repeating_status = "Repeating instrument")
}

get_codebook_validation_label <- function(
  validation_type,
  validation_min,
  validation_max
) {
  validation_type <- as.character(validation_type)
  validation_min <- as.character(validation_min)
  validation_max <- as.character(validation_max)

  map_chr(seq_along(validation_type), \(index) {
    pieces <- c(
      validation_type[[index]],
      if (!get_is_missing(validation_min[[index]])) {
        paste("min", validation_min[[index]])
      } else {
        NA_character_
      },
      if (!get_is_missing(validation_max[[index]])) {
        paste("max", validation_max[[index]])
      } else {
        NA_character_
      }
    )
    pieces <- pieces[!get_is_missing(pieces)]

    if (length(pieces) == 0) {
      return(NA_character_)
    }

    paste(pieces, collapse = "; ")
  })
}

get_codebook_event_count <- function(project) {
  if (
    nrow(project$events) == 0 || !"redcap_event_name" %in% names(project$events)
  ) {
    return(NA_integer_)
  }

  n_distinct(project$events$redcap_event_name)
}

get_codebook_repeating_enabled <- function(project) {
  repeating <- project$repeating_instruments
  nrow(repeating) > 0
}

get_validate_codebook <- function(codebook) {
  if (!inherits(codebook, "redcap_codebook")) {
    cli_abort("{.arg codebook} must be a {.cls redcap_codebook} object.")
  }

  invisible(NULL)
}

get_validate_page_length <- function(page_length) {
  valid_page_length <- is.numeric(page_length) &&
    length(page_length) == 1 &&
    !is.na(page_length) &&
    page_length > 0 &&
    page_length == as.integer(page_length)

  if (!valid_page_length) {
    cli_abort("{.arg page_length} must be a positive integer.")
  }

  invisible(NULL)
}

get_codebook_viewer_tables <- function(codebook) {
  tables <- list(
    project = list(
      title = "Project",
      data = codebook$project,
      description = paste(
        "A one-row overview of the REDCap project structure. Use this table",
        "to quickly confirm the project title and high-level counts for",
        "fields, forms, events, and repeating configuration."
      )
    ),
    fields = list(
      title = "Fields",
      data = codebook$fields,
      description = paste(
        "One row per REDCap field in data dictionary order. Read across a row",
        "to see the field's form, label, type, required status, choices,",
        "validation rules, branching logic, event use, and repeating status."
      )
    ),
    choices = list(
      title = "Choices",
      data = codebook$choices,
      description = paste(
        "One row per coded option from choice-style fields such as radio,",
        "dropdown, checkbox, yes/no, and true/false fields. Use this table",
        "to review stored values alongside their display labels."
      )
    ),
    forms = list(
      title = "Forms",
      data = codebook$forms,
      description = paste(
        "One row per REDCap instrument or form in project order. Use this",
        "table to review form labels and the number of fields assigned to",
        "each instrument."
      )
    ),
    events = list(
      title = "Events",
      data = codebook$events,
      description = paste(
        "One row per REDCap event. Use this table to review event names,",
        "unique event identifiers, and arm information for longitudinal",
        "project schedules."
      )
    ),
    event_instruments = list(
      title = "Event Instruments",
      data = codebook$event_instruments,
      description = paste(
        "One row per event and instrument pairing. Use this table to see",
        "which forms are expected or enabled at each longitudinal event."
      )
    ),
    repeating = list(
      title = "Repeating",
      data = codebook$repeating,
      description = paste(
        "One row per repeating event or repeating instrument configuration.",
        "Use this table to identify which parts of the project can collect",
        "multiple instances for the same record."
      )
    )
  )

  get_codebook_visible_tables(tables)
}

get_codebook_visible_tables <- function(tables) {
  optional_sections <- c("events", "event_instruments", "repeating")

  keep <- map_lgl(names(tables), \(section_id) {
    !section_id %in% optional_sections ||
      get_codebook_viewer_has_rows(tables[[section_id]]$data)
  })

  tables[keep]
}

get_codebook_viewer_has_rows <- function(data) {
  !is.null(data) && nrow(as_tibble(data)) > 0
}

get_codebook_viewer_summary <- function(codebook) {
  project <- codebook$project
  summary_items <- list(
    Fields = project$field_count[[1]],
    Forms = project$form_count[[1]],
    Repeating = if (project$repeating_enabled[[1]]) "Yes" else "No"
  )

  if (!is.na(project$event_count[[1]]) && project$event_count[[1]] > 0) {
    summary_items <- append(
      summary_items,
      list(Events = project$event_count[[1]]),
      after = 2
    )
  }

  tags$dl(
    class = "redcap-codebook-summary",
    map(names(summary_items), \(label) {
      tags$div(
        class = "redcap-codebook-summary-item",
        tags$dt(label),
        tags$dd(as.character(summary_items[[label]]))
      )
    })
  )
}

get_codebook_viewer_navigation <- function(codebook) {
  tables <- get_codebook_viewer_tables(codebook)

  tags$nav(
    class = "redcap-codebook-nav",
    role = "tablist",
    tags$ul(
      map(seq_along(tables), \(index) {
        section_id <- names(tables)[[index]]
        tags$li(
          tags$button(
            type = "button",
            class = paste(
              "redcap-codebook-tab",
              if (index == 1) "is-active" else ""
            ),
            `data-section` = section_id,
            role = "tab",
            `aria-controls` = section_id,
            `aria-selected` = if (index == 1) "true" else "false",
            tags$span(
              class = "redcap-codebook-tab-label",
              tables[[section_id]]$title
            ),
            tags$span(
              class = "redcap-codebook-tab-count",
              get_codebook_viewer_row_label(tables[[section_id]]$data)
            )
          )
        )
      })
    )
  )
}

get_codebook_viewer_sections <- function(codebook, page_length) {
  tables <- get_codebook_viewer_tables(codebook)

  tagList(map(seq_along(tables), \(index) {
    section_id <- names(tables)[[index]]
    section <- tables[[section_id]]
    data <- get_codebook_viewer_data(section$data)

    tags$section(
      id = section_id,
      class = paste(
        "redcap-codebook-section",
        if (index == 1) "is-active" else ""
      ),
      role = "tabpanel",
      tags$div(
        class = "redcap-codebook-section-header",
        tags$div(
          tags$h2(section$title),
          tags$p(class = "redcap-codebook-description", section$description)
        ),
        tags$span(
          class = "redcap-codebook-section-count",
          get_codebook_viewer_row_label(section$data)
        )
      ),
      tags$div(
        class = "redcap-codebook-table-panel",
        if (section_id == "project") {
          get_codebook_project_details(section$data)
        } else if (nrow(data) == 0) {
          tags$p(
            class = "redcap-codebook-empty",
            get_codebook_empty_message(section_id)
          )
        } else {
          get_codebook_datatable(data, page_length)
        }
      )
    )
  }))
}

get_codebook_project_details <- function(project) {
  detail_groups <- get_project_detail_groups()

  tags$div(
    class = "redcap-codebook-project-details",
    map(names(detail_groups), \(group_name) {
      rows <- get_project_detail_rows(
        project,
        detail_groups[[group_name]]
      )
      if (nrow(rows) == 0) {
        return(NULL)
      }

      tags$div(
        class = "redcap-codebook-project-detail-group",
        tags$h3(group_name),
        tags$table(
          class = "redcap-codebook-project-detail-table",
          tags$tbody(
            map(seq_len(nrow(rows)), \(index) {
              tags$tr(
                tags$th(scope = "row", rows$label[[index]]),
                tags$td(rows$value[[index]])
              )
            })
          )
        )
      )
    })
  )
}

get_project_detail_groups <- function() {
  list(
    Overview = c(
      "Project title" = "project_title",
      "Project ID" = "project_id",
      "Fields" = "field_count",
      "Forms" = "form_count",
      "Events" = "event_count",
      "Repeating enabled" = "repeating_enabled",
      "Language" = "project_language",
      "Created" = "creation_time",
      "Production status" = "in_production",
      "Production time" = "production_time"
    ),
    `Project features` = c(
      "Longitudinal" = "is_longitudinal",
      "Repeating instruments or events" = "has_repeating_instruments_or_events",
      "Surveys enabled" = "surveys_enabled",
      "Scheduling enabled" = "scheduling_enabled",
      "Record autonumbering enabled" = "record_autonumbering_enabled",
      "Randomization enabled" = "randomization_enabled",
      "DDP enabled" = "ddp_enabled"
    ),
    `Labels and configuration` = c(
      "Custom record label" = "custom_record_label",
      "Secondary unique field" = "secondary_unique_field",
      "Missing data codes" = "missing_data_codes",
      "External modules" = "external_modules"
    ),
    `Administrative metadata` = c(
      "Purpose" = "purpose",
      "Purpose other" = "purpose_other",
      "Project notes" = "project_notes",
      "IRB number" = "project_irb_number",
      "Grant number" = "project_grant_number",
      "PI first name" = "project_pi_firstname",
      "PI last name" = "project_pi_lastname",
      "PI email" = "project_pi_email"
    )
  )
}

get_project_detail_rows <- function(project, fields) {
  bind_rows(map(names(fields), \(label) {
    field <- fields[[label]]
    if (!field %in% names(project)) {
      return(tibble())
    }

    value <- project[[field]][[1]]
    if (get_is_missing(value)) {
      return(tibble())
    }

    tibble(
      label = label,
      value = get_project_detail_value(value)
    )
  }))
}

get_project_detail_value <- function(value) {
  if (is.logical(value)) {
    return(if (value) "Yes" else "No")
  }

  get_codebook_viewer_cell(value)
}

get_codebook_viewer_row_label <- function(data) {
  row_count <- nrow(as_tibble(data))
  paste(row_count, if (row_count == 1) "row" else "rows")
}

get_codebook_empty_message <- function(section_id) {
  switch(
    section_id,
    choices = "No coded choice options were found in this project.",
    "No rows are available for this section."
  )
}

get_codebook_viewer_data <- function(data) {
  if (is.null(data)) {
    return(tibble())
  }

  data <- as_tibble(data)
  mutate(data, across(everything(), get_codebook_viewer_cell))
}

get_codebook_viewer_cell <- function(value) {
  if (is.list(value) && !is.atomic(value)) {
    return(map_chr(value, \(item) {
      paste(as.character(item), collapse = ", ")
    }))
  }

  as.character(value)
}

get_codebook_datatable <- function(data, page_length) {
  datatable(
    data,
    filter = "top",
    class = "display compact stripe hover redcap-codebook-table",
    rownames = FALSE,
    escape = TRUE,
    options = list(
      pageLength = as.integer(page_length),
      scrollX = TRUE
    )
  )
}

get_codebook_viewer_deps <- function() {
  tags$head(
    tags$style(HTML(
      "
      .redcap-codebook-viewer {
        background: #f6f8fa;
        color: #24292f;
        font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI',
          sans-serif;
        line-height: 1.4;
        margin: 0;
        min-height: 100vh;
        padding: 1.25rem;
      }
      .redcap-codebook-header {
        background: #ffffff;
        border: 1px solid #d0d7de;
        border-radius: 6px;
        margin-bottom: 1rem;
        padding: 1rem 1.25rem;
      }
      .redcap-codebook-header h1 {
        margin-bottom: 0.25rem;
      }
      .redcap-codebook-subtitle {
        color: #57606a;
        margin-top: 0;
      }
      .redcap-codebook-summary {
        display: flex;
        flex-wrap: wrap;
        gap: 0.75rem;
        margin: 1rem 0 0;
      }
      .redcap-codebook-summary-item {
        background: #f6f8fa;
        border: 1px solid #d8dee4;
        border-radius: 6px;
        min-width: 7rem;
        padding: 0.5rem 0.75rem;
      }
      .redcap-codebook-summary-item dt {
        color: #57606a;
        font-size: 0.75rem;
        font-weight: 600;
        margin-bottom: 0.15rem;
        text-transform: uppercase;
      }
      .redcap-codebook-summary-item dd {
        font-size: 1.2rem;
        font-weight: 650;
        margin: 0;
      }
      .redcap-codebook-nav {
        background: #f6f8fa;
        border-bottom: 1px solid #d8dee4;
        margin: 0 -1.25rem 1rem;
        padding: 0 1.25rem 0.75rem;
        position: sticky;
        top: 0;
        z-index: 10;
      }
      .redcap-codebook-nav ul {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
        list-style: none;
        padding-left: 0;
        margin: 0;
      }
      .redcap-codebook-tab {
        background: #ffffff;
        align-items: center;
        display: inline-block;
        border: 1px solid #d0d7de;
        border-radius: 6px;
        color: #24292f;
        cursor: pointer;
        display: inline-flex;
        gap: 0.4rem;
        font: inherit;
        font-size: 0.9rem;
        padding: 0.4rem 0.65rem;
      }
      .redcap-codebook-tab.is-active {
        background: #0969da;
        border-color: #0969da;
        color: #ffffff;
      }
      .redcap-codebook-tab-count {
        background: #f6f8fa;
        border-radius: 999px;
        color: #57606a;
        font-size: 0.75rem;
        padding: 0.05rem 0.45rem;
      }
      .redcap-codebook-tab.is-active .redcap-codebook-tab-count {
        background: rgba(255, 255, 255, 0.18);
        color: #ffffff;
      }
      .redcap-codebook-section {
        height: 0;
        opacity: 0;
        overflow: hidden;
        pointer-events: none;
        position: absolute;
        visibility: hidden;
        width: 100%;
      }
      .redcap-codebook-section.is-active {
        height: auto;
        opacity: 1;
        overflow: visible;
        pointer-events: auto;
        position: static;
        visibility: visible;
      }
      .redcap-codebook-section-header {
        align-items: flex-start;
        display: flex;
        gap: 1rem;
        justify-content: space-between;
        margin-bottom: 0.75rem;
      }
      .redcap-codebook-section-header h2 {
        margin: 0 0 0.3rem;
      }
      .redcap-codebook-section-count {
        background: #eaeef2;
        border-radius: 999px;
        color: #57606a;
        flex: 0 0 auto;
        font-size: 0.8rem;
        margin-top: 0.15rem;
        padding: 0.15rem 0.55rem;
      }
      .redcap-codebook-description {
        color: #57606a;
        margin: 0;
        max-width: 72rem;
      }
      .redcap-codebook-table-panel {
        background: #ffffff;
        border: 1px solid #d0d7de;
        border-radius: 6px;
        overflow-x: auto;
        padding: 0.75rem;
      }
      .redcap-codebook-empty {
        color: #57606a;
        font-style: italic;
        margin: 0;
      }
      .redcap-codebook-project-details {
        display: grid;
        gap: 1rem;
        grid-template-columns: repeat(auto-fit, minmax(18rem, 1fr));
      }
      .redcap-codebook-project-detail-group {
        border: 1px solid #d8dee4;
        border-radius: 6px;
        overflow: hidden;
      }
      .redcap-codebook-project-detail-group h3 {
        background: #f6f8fa;
        border-bottom: 1px solid #d8dee4;
        font-size: 0.95rem;
        margin: 0;
        padding: 0.5rem 0.75rem;
      }
      .redcap-codebook-project-detail-table {
        border-collapse: collapse;
        font-size: 0.88rem;
        width: 100%;
      }
      .redcap-codebook-project-detail-table th,
      .redcap-codebook-project-detail-table td {
        border-bottom: 1px solid #eaeef2;
        padding: 0.45rem 0.75rem;
        text-align: left;
        vertical-align: top;
      }
      .redcap-codebook-project-detail-table th {
        color: #57606a;
        font-weight: 600;
        width: 42%;
      }
      .redcap-codebook-project-detail-table tr:last-child th,
      .redcap-codebook-project-detail-table tr:last-child td {
        border-bottom: 0;
      }
      .redcap-codebook-table {
        font-size: 0.86rem;
      }
      .redcap-codebook-table thead th {
        background: #f6f8fa;
        border-bottom-color: #d0d7de;
        color: #24292f;
      }
      .redcap-codebook-table tbody td {
        vertical-align: top;
      }
      .redcap-codebook-table tbody tr {
        cursor: pointer;
      }
      .redcap-codebook-table tbody tr.redcap-codebook-row-selected td {
        background-color: #fff8c5 !important;
      }
      .redcap-codebook-table-panel .dataTables_wrapper {
        color: #24292f;
      }
      .redcap-codebook-table-panel .dataTables_filter input,
      .redcap-codebook-table-panel .dataTables_length select,
      .redcap-codebook-table-panel thead input {
        border: 1px solid #d0d7de;
        border-radius: 4px;
        padding: 0.2rem 0.35rem;
      }
      .redcap-codebook-table-panel .dataTables_info,
      .redcap-codebook-table-panel .dataTables_paginate {
        color: #57606a;
        font-size: 0.85rem;
        margin-top: 0.5rem;
      }
      "
    )),
    tags$script(HTML(
      "
      document.addEventListener('DOMContentLoaded', function() {
        var tabs = document.querySelectorAll('.redcap-codebook-tab');
        var sections = document.querySelectorAll('.redcap-codebook-section');

        function showSection(sectionId) {
          tabs.forEach(function(tab) {
            var active = tab.getAttribute('data-section') === sectionId;
            tab.classList.toggle('is-active', active);
            tab.setAttribute('aria-selected', active ? 'true' : 'false');
          });

          sections.forEach(function(section) {
            section.classList.toggle('is-active', section.id === sectionId);
          });

          if (window.HTMLWidgets && window.HTMLWidgets.staticRender) {
            window.HTMLWidgets.staticRender();
          }

          if (window.jQuery && window.jQuery.fn.dataTable) {
            window.jQuery('.dataTable').each(function() {
              if (window.jQuery.fn.dataTable.isDataTable(this)) {
                window.jQuery(this).DataTable().columns.adjust();
              }
            });
          }
        }

        tabs.forEach(function(tab) {
          tab.addEventListener('click', function(event) {
            event.preventDefault();
            showSection(tab.getAttribute('data-section'));
            document.querySelector('.redcap-codebook-header')
              .scrollIntoView({ behavior: 'auto', block: 'start' });
          });
        });

        document.addEventListener('click', function(event) {
          var row = event.target.closest('.redcap-codebook-table tbody tr');
          if (!row) {
            return;
          }

          var tbody = row.parentElement;
          tbody.querySelectorAll('tr.redcap-codebook-row-selected')
            .forEach(function(selectedRow) {
              selectedRow.classList.remove('redcap-codebook-row-selected');
            });
          row.classList.add('redcap-codebook-row-selected');
        });

        if (window.location.hash) {
          var sectionId = window.location.hash.replace('#', '');
          if (document.getElementById(sectionId)) {
            showSection(sectionId);
          }
        }
      });
      "
    ))
  )
}
