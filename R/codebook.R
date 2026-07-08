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
#' [htmltools::save_html()].
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
          tags$h1("REDCap Codebook"),
          tags$p(
            "Use the section buttons to navigate between codebook tables."
          )
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
      field_note = if ("field_note" %in% names(metadata))
        as.character(.data$field_note) else NA_character_,
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
    field_count = n_distinct(project$metadata$field_name),
    form_count = n_distinct(project$metadata$form_name),
    event_count = get_codebook_event_count(project),
    repeating_enabled = get_codebook_repeating_enabled(project)
  )
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
      if (!get_is_missing(validation_min[[index]]))
        paste("min", validation_min[[index]]) else NA_character_,
      if (!get_is_missing(validation_max[[index]]))
        paste("max", validation_max[[index]]) else NA_character_
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
  list(
    project = list(
      title = "Project",
      data = codebook$project,
      description = paste(
        "One row of project-level summary metadata, including project title,",
        "field count, form count, event count, and repeating status."
      )
    ),
    fields = list(
      title = "Fields",
      data = codebook$fields,
      description = paste(
        "One row per REDCap field with display-oriented metadata, parsed",
        "choice summaries, validation, branching logic, event applicability,",
        "and repeating status when available."
      )
    ),
    choices = list(
      title = "Choices",
      data = codebook$choices,
      description = paste(
        "One row per parsed option from radio, dropdown, checkbox, and other",
        "choice-style REDCap fields."
      )
    ),
    forms = list(
      title = "Forms",
      data = codebook$forms,
      description = "One row per REDCap instrument or form in project order."
    ),
    events = list(
      title = "Events",
      data = codebook$events,
      description = paste(
        "One row per REDCap event when the project is longitudinal; empty for",
        "classic projects."
      )
    ),
    event_instruments = list(
      title = "Event Instruments",
      data = codebook$event_instruments,
      description = paste(
        "One row per event-form mapping when event/instrument mapping is",
        "available from the REDCap API."
      )
    ),
    repeating = list(
      title = "Repeating",
      data = codebook$repeating,
      description = paste(
        "Repeating instrument and repeating event configuration when available",
        "from the REDCap API."
      )
    )
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
            tables[[section_id]]$title
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
      tags$h2(section$title),
      tags$p(class = "redcap-codebook-description", section$description),
      if (nrow(data) == 0) {
        tags$p(
          class = "redcap-codebook-empty",
          "No rows available."
        )
      } else {
        get_codebook_datatable(data, page_length)
      }
    )
  }))
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
        font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI',
          sans-serif;
        margin: 1rem;
      }
      .redcap-codebook-header {
        margin-bottom: 1rem;
      }
      .redcap-codebook-header h1 {
        margin-bottom: 0.25rem;
      }
      .redcap-codebook-nav ul {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
        list-style: none;
        padding-left: 0;
        margin-bottom: 1.5rem;
      }
      .redcap-codebook-tab {
        background: #ffffff;
        display: inline-block;
        border: 1px solid #d0d7de;
        border-radius: 4px;
        color: #0969da;
        cursor: pointer;
        font: inherit;
        padding: 0.25rem 0.5rem;
      }
      .redcap-codebook-tab.is-active {
        background: #0969da;
        border-color: #0969da;
        color: #ffffff;
      }
      .redcap-codebook-section {
        border-top: 1px solid #d0d7de;
        margin-top: 1.5rem;
        padding-top: 1rem;
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
      .redcap-codebook-description {
        color: #57606a;
        max-width: 72rem;
      }
      .redcap-codebook-empty {
        color: #57606a;
        font-style: italic;
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
