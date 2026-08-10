#' @title View a REDCap quality report as interactive HTML
#'
#' @description
#' `view_quality_report()` turns a `redcap_quality_report` object from
#' [build_quality_report()] into a static HTML viewer with project metrics,
#' findings, summary tables, and metadata reference tables. The returned object
#' prints in the RStudio/Posit Viewer. Use [save_quality_report()] to save a
#' single HTML file for sharing.
#'
#' @param report A `redcap_quality_report` object returned by
#'   [build_quality_report()].
#' @param page_length Number of rows to show per interactive table page.
#'
#' @returns An `htmltools` browsable HTML object containing interactive `DT`
#'   tables.
#'
#' @examples
#' view_quality_report(mock_quality_report)
#'
#' @export
view_quality_report <- function(report, page_length = 25) {
  get_validate_quality_report(report)
  get_validate_page_length(page_length)

  browsable(
    tagList(
      get_quality_report_viewer_deps(),
      tags$main(
        class = "redcap-quality-report-viewer",
        tags$header(
          id = "quality-report-top",
          class = "redcap-quality-report-header",
          tags$h1("REDCap Quality Report"),
          tags$p(
            class = "redcap-quality-report-subtitle",
            "Interactive findings review"
          ),
          tags$p(
            class = "redcap-quality-report-intro",
            paste(
              "Review project-level metrics, filter actionable findings, and",
              "inspect the summaries and metadata used to interpret the report."
            )
          )
        ),
        get_quality_report_primary_navigation(report),
        get_quality_report_primary_sections(report, page_length)
      )
    )
  )
}

#' @title Save a REDCap quality report viewer
#'
#' @description
#' `save_quality_report()` saves the interactive HTML viewer from
#' [view_quality_report()] as a single HTML file. Local CSS and JavaScript
#' dependencies are inlined so the file can be opened in a browser or shared
#' without a separate dependency folder.
#'
#' @param report A `redcap_quality_report` object returned by
#'   [build_quality_report()].
#' @param file Output HTML file path.
#' @param page_length Number of rows to show per interactive table page.
#'
#' @returns The output file path, invisibly.
#'
#' @examples
#' \dontrun{
#' save_quality_report(mock_quality_report, "quality-report.html")
#' }
#'
#' @export
save_quality_report <- function(report, file, page_length = 25) {
  get_validate_quality_report(report)
  get_validate_html_file(file)
  get_validate_page_length(page_length)

  viewer <- view_quality_report(report, page_length = page_length)
  get_save_html_single_file(
    viewer,
    file,
    source = "redcap-quality-report"
  )

  invisible(file)
}

get_validate_quality_report <- function(report) {
  if (!inherits(report, "redcap_quality_report")) {
    cli_abort(
      "{.arg report} must be a {.cls redcap_quality_report} object."
    )
  }

  invisible(NULL)
}

get_quality_report_primary_navigation <- function(report) {
  items <- list(
    overview = list(title = "Overview", count = "8 metrics"),
    findings = list(
      title = "Findings",
      count = get_quality_report_row_label(report$findings)
    ),
    summaries = list(
      title = "Summaries",
      count = paste(length(get_quality_report_summary_tables(report)), "tables")
    ),
    metadata = list(
      title = "Metadata",
      count = paste(length(get_quality_report_metadata_tables(report)), "tables")
    )
  )

  get_quality_report_tab_navigation(
    items = items,
    group = "primary",
    class = "redcap-quality-report-primary-nav"
  )
}

get_quality_report_tab_navigation <- function(items, group, class) {
  tags$nav(
    class = paste("redcap-quality-report-nav", class),
    role = "tablist",
    tags$ul(
      map(seq_along(items), \(index) {
        section_id <- names(items)[[index]]
        item <- items[[section_id]]
        tags$li(
          tags$button(
            type = "button",
            class = paste(
              "redcap-quality-report-tab",
              if (index == 1) "is-active" else ""
            ),
            `data-tab-group` = group,
            `data-section` = section_id,
            role = "tab",
            `aria-controls` = section_id,
            `aria-selected` = if (index == 1) "true" else "false",
            tags$span(class = "redcap-quality-report-tab-label", item$title),
            tags$span(class = "redcap-quality-report-tab-count", item$count)
          )
        )
      })
    )
  )
}

get_quality_report_primary_sections <- function(report, page_length) {
  tagList(
    get_quality_report_overview(report),
    get_quality_report_findings(report, page_length),
    get_quality_report_table_group(
      title = "Summaries",
      description = paste(
        "Aggregate views of missingness, findings, project structure, and",
        "record-level review burden."
      ),
      tables = get_quality_report_summary_tables(report),
      group = "summary",
      page_length = page_length
    ),
    get_quality_report_table_group(
      title = "Metadata",
      description = paste(
        "Reference tables describing fields, forms, choices, validation,",
        "branching logic, and longitudinal or repeating project structure."
      ),
      tables = get_quality_report_metadata_tables(report),
      group = "metadata",
      page_length = page_length
    )
  )
}

get_quality_report_primary_section <- function(id, ..., active = FALSE) {
  tags$section(
    id = id,
    class = paste(
      "redcap-quality-report-primary-section",
      if (active) "is-active" else ""
    ),
    `data-panel-group` = "primary",
    role = "tabpanel",
    ...
  )
}

get_quality_report_overview <- function(report) {
  project <- report$summaries$project

  get_quality_report_primary_section(
    "overview",
    active = TRUE,
    tags$div(
      class = "redcap-quality-report-section-header",
      tags$div(
        tags$h2("Overview"),
        tags$p(
          class = "redcap-quality-report-description",
          paste(
            "Project-level counts and weighted missingness from the current",
            "quality report."
          )
        )
      )
    ),
    get_quality_report_summary_cards(project),
    get_quality_report_details(report)
  )
}

get_quality_report_summary_cards <- function(project) {
  values <- list(
    Records = project$record_count[[1]],
    `Raw rows` = project$raw_row_count[[1]],
    Fields = project$field_count[[1]],
    Forms = project$form_count[[1]],
    Events = project$event_count[[1]],
    Findings = project$finding_count[[1]],
    `Missing rate` = get_quality_report_percent(project$missing_rate[[1]]),
    Repeating = if (project$repeating_enabled[[1]]) "Yes" else "No"
  )

  tags$dl(
    class = "redcap-quality-report-summary",
    map(names(values), \(label) {
      tags$div(
        class = "redcap-quality-report-summary-item",
        tags$dt(label),
        tags$dd(get_quality_report_display_value(values[[label]]))
      )
    })
  )
}

get_quality_report_details <- function(report) {
  details <- list(
    `Record ID field` = report$metadata$record_id_field,
    Source = report$metadata$source
  )
  details <- details[map_lgl(details, \(value) {
    !is.null(value) && length(value) == 1 && !get_is_missing(value)
  })]

  if (length(details) == 0) {
    return(NULL)
  }

  tags$div(
    class = "redcap-quality-report-details",
    tags$h3("Report details"),
    tags$dl(
      map(names(details), \(label) {
        tagList(
          tags$dt(label),
          tags$dd(as.character(details[[label]]))
        )
      })
    )
  )
}

get_quality_report_findings <- function(report, page_length) {
  findings <- get_quality_report_viewer_data(report$findings)

  get_quality_report_primary_section(
    "findings",
    tags$div(
      class = "redcap-quality-report-section-header",
      tags$div(
        tags$h2("Findings"),
        tags$p(
          class = "redcap-quality-report-description",
          paste(
            "One row per detected issue, including its check, severity, scope,",
            "REDCap context, observed value, expectation, and review message."
          )
        )
      ),
      tags$span(
        class = "redcap-quality-report-section-count",
        get_quality_report_row_label(report$findings)
      )
    ),
    tags$div(
      class = "redcap-quality-report-table-panel",
      if (nrow(findings) == 0) {
        tags$p(
          class = "redcap-quality-report-empty",
          "No findings were generated by the selected quality checks."
        )
      } else {
        get_quality_report_datatable(findings, page_length)
      }
    )
  )
}

get_quality_report_table_group <- function(
  title,
  description,
  tables,
  group,
  page_length
) {
  navigation_items <- map(tables, \(table) {
    list(
      title = table$title,
      count = get_quality_report_row_label(table$data)
    )
  })

  get_quality_report_primary_section(
    if (group == "summary") "summaries" else "metadata",
    tags$div(
      class = "redcap-quality-report-section-header",
      tags$div(
        tags$h2(title),
        tags$p(class = "redcap-quality-report-description", description)
      )
    ),
    get_quality_report_tab_navigation(
      navigation_items,
      group = group,
      class = "redcap-quality-report-secondary-nav"
    ),
    tagList(map(seq_along(tables), \(index) {
      section_id <- names(tables)[[index]]
      table <- tables[[section_id]]
      data <- get_quality_report_viewer_data(table$data)

      tags$section(
        id = section_id,
        class = paste(
          "redcap-quality-report-table-section",
          if (index == 1) "is-active" else ""
        ),
        `data-panel-group` = group,
        role = "tabpanel",
        tags$div(
          class = "redcap-quality-report-section-header",
          tags$div(
            tags$h3(table$title),
            tags$p(
              class = "redcap-quality-report-description",
              table$description
            )
          ),
          tags$span(
            class = "redcap-quality-report-section-count",
            get_quality_report_row_label(table$data)
          )
        ),
        tags$div(
          class = "redcap-quality-report-table-panel",
          if (nrow(data) == 0) {
            tags$p(
              class = "redcap-quality-report-empty",
              table$empty_message
            )
          } else {
            get_quality_report_datatable(data, page_length)
          }
        )
      )
    }))
  )
}

get_quality_report_summary_tables <- function(report) {
  tables <- list(
    `summary-forms` = list(
      title = "Forms",
      data = report$summaries$forms,
      description = paste(
        "One row per form with field counts, required-field counts, and the",
        "weighted fraction missing across applicable field-row cells."
      ),
      empty_message = "No form summaries are available."
    ),
    `summary-fields` = list(
      title = "Fields",
      data = report$summaries$fields,
      description = paste(
        "One row per represented field with applicable record counts,",
        "missingness, observed values, and distinct-value counts."
      ),
      empty_message = "No field summaries are available."
    ),
    `summary-records` = list(
      title = "Records",
      data = report$summaries$records,
      description = paste(
        "One row per record with its number of missing applicable fields and",
        "total findings across event and repeat rows."
      ),
      empty_message = "No record summaries are available."
    ),
    `summary-events` = list(
      title = "Events",
      data = report$summaries$events,
      description = "One row per represented REDCap event with its exported row count.",
      empty_message = "No event summaries are available for this project."
    )
  )

  get_quality_report_visible_tables(tables, optional = "summary-events")
}

get_quality_report_metadata_tables <- function(report) {
  tables <- list(
    `metadata-fields` = list(
      title = "Fields",
      data = report$metadata$fields,
      description = paste(
        "One row per REDCap field with its form, field type, label, choices,",
        "validation settings, required status, branching logic, and identifier flag."
      ),
      empty_message = "No field metadata are available."
    ),
    `metadata-forms` = list(
      title = "Forms",
      data = report$metadata$forms,
      description = "One row per form with field, required-field, and choice-field counts.",
      empty_message = "No form metadata are available."
    ),
    `metadata-instruments` = list(
      title = "Instruments",
      data = report$metadata$instruments,
      description = "One row per REDCap instrument with its machine name and display label.",
      empty_message = "No instrument metadata are available."
    ),
    `metadata-events` = list(
      title = "Events",
      data = report$metadata$events,
      description = "One row per REDCap event with unique event and arm context.",
      empty_message = "No event metadata are available for this project."
    ),
    `metadata-event-instruments` = list(
      title = "Event Instruments",
      data = report$metadata$event_instruments,
      description = "One row per event and instrument mapping in a longitudinal project.",
      empty_message = "No event-instrument mappings are available."
    ),
    `metadata-repeating` = list(
      title = "Repeating",
      data = report$metadata$repeating_instruments,
      description = "One row per repeating event or repeating instrument configuration.",
      empty_message = "No repeating configuration is available."
    ),
    `metadata-choices` = list(
      title = "Choices",
      data = report$metadata$choices,
      description = "One row per coded choice value and display label.",
      empty_message = "No coded choice metadata are available."
    ),
    `metadata-validation` = list(
      title = "Validation",
      data = report$metadata$validation,
      description = "One row per field with configured text validation or range limits.",
      empty_message = "No validation metadata are available."
    ),
    `metadata-branching` = list(
      title = "Branching",
      data = report$metadata$branching,
      description = "One row per field with branching logic and its referenced fields.",
      empty_message = "No branching-logic metadata are available."
    )
  )

  get_quality_report_visible_tables(
    tables,
    optional = c(
      "metadata-events",
      "metadata-event-instruments",
      "metadata-repeating"
    )
  )
}

get_quality_report_visible_tables <- function(tables, optional) {
  keep <- map_lgl(names(tables), \(section_id) {
    !section_id %in% optional || get_quality_report_has_rows(tables[[section_id]]$data)
  })

  tables[keep]
}

get_quality_report_has_rows <- function(data) {
  !is.null(data) && nrow(as_tibble(data)) > 0
}

get_quality_report_viewer_data <- function(data) {
  if (is.null(data)) {
    return(tibble())
  }

  data <- as_tibble(data)
  rate_columns <- names(data)[endsWith(names(data), "_rate")]
  if (length(rate_columns) > 0) {
    data <- data |>
      mutate(across(all_of(rate_columns), get_quality_report_percent))
  }

  data <- data |>
    mutate(across(everything(), get_quality_report_viewer_cell))

  factor_columns <- intersect(
    names(data),
    get_quality_report_factor_columns()
  )

  mutate(data, across(all_of(factor_columns), as.factor))
}

get_quality_report_factor_columns <- function() {
  c(
    "check",
    "issue",
    "severity",
    "scope",
    "form_name",
    "event_name",
    "repeat_instrument",
    "field_type",
    "required_field",
    "identifier",
    "text_validation_type_or_show_slider_number",
    "choice_value",
    "instrument",
    "arm_num",
    "redcap_arm"
  )
}

get_quality_report_viewer_cell <- function(value) {
  if (is.list(value) && !is.atomic(value)) {
    return(map_chr(value, \(item) {
      paste(as.character(item), collapse = ", ")
    }))
  }

  as.character(value)
}

get_quality_report_percent <- function(value) {
  if_else(
    is.na(value),
    NA_character_,
    paste0(format(round(value * 100, 1), nsmall = 1, trim = TRUE), "%")
  )
}

get_quality_report_display_value <- function(value) {
  if (length(value) == 0 || get_is_missing(value)) {
    return("Not available")
  }

  as.character(value)
}

get_quality_report_row_label <- function(data) {
  row_count <- if (is.null(data)) 0 else nrow(as_tibble(data))
  paste(row_count, if (row_count == 1) "row" else "rows")
}

get_quality_report_datatable <- function(data, page_length) {
  datatable(
    data,
    filter = "top",
    class = "display compact stripe hover redcap-quality-report-table",
    rownames = FALSE,
    escape = TRUE,
    options = list(
      pageLength = as.integer(page_length),
      scrollX = TRUE
    )
  )
}

get_quality_report_viewer_deps <- function() {
  htmlDependency(
    name = "redcap-quality-report-viewer",
    version = "1.0.0",
    src = c(
      file = system.file("quality-report-viewer", package = "REDCapExploreR")
    ),
    stylesheet = "quality-report.css",
    script = "quality-report.js"
  )
}
