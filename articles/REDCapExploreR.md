# Getting Started with REDCapExploreR

REDCapExploreR provides exploratory tools for REDCap projects accessed
through the REDCap API. Use it to inspect project structure, review form
completion, and identify general data quality findings without modifying
the REDCap project.

## Installation

Install the development version from GitHub and load the package:

``` r

devtools::install_github("CHOP-CGTInformatics/REDCapExploreR")
library(REDCapExploreR)
```

## REDCap API credentials

Functions that retrieve a REDCap project need an API endpoint and a
project token with appropriate API permissions. Store credentials
outside version-controlled code, such as in environment variables:

``` r

redcap_uri <- Sys.getenv("REDCAP_URI")
token <- Sys.getenv("REDCAP_TOKEN")
```

The examples below assume `redcap_uri` and `token` are set. The package
also includes synthetic objects for credential-free exploration.

## Choose a workflow

The primary workflows are independent. Start with the function that
matches your review goal.

| Goal | Start with |
|----|----|
| Understand project fields and structure | [`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md) |
| Review form completion | [`build_record_status_data()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_record_status_data.md) |
| Review data quality findings | [`build_quality_report()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_quality_report.md) |
| Analyze the underlying API tables directly | [`pull_redcap_project()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/pull_redcap_project.md) |

## Codebook

[`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md)
returns a structured view of project fields, choices, forms, events,
repeating configuration, and project-level metadata. It retrieves
metadata only: counts describe configured structure, not populated
records. Expressions such as branching logic and calculations are
retained without being evaluated.

``` r

codebook <- build_codebook(
  redcap_uri = redcap_uri,
  token = token
)

codebook
codebook$fields
```

Use `mock_codebook` to inspect the output structure without API
credentials.

``` r

mock_codebook
#> <REDCap codebook>
#> Project: Mock REDCap Database
#> Forms: 3
#> Fields: 21
#> Events: 2
#> Repeating enabled: TRUE
head(mock_codebook$fields)
#> # A tibble: 6 × 19
#>   field_order form_order form_name  form_label field_name field_label field_type
#>         <int>      <int> <chr>      <chr>      <chr>      <chr>       <chr>     
#> 1           1          1 demograph… Demograph… record_id  Record ID   text      
#> 2           2          1 demograph… Demograph… age        Age at enr… text      
#> 3           3          1 demograph… Demograph… enrollmen… Enrollment… text      
#> 4           4          1 demograph… Demograph… sex        Sex         radio     
#> 5           5          2 follow_up  Follow-up  visit_date Visit date  text      
#> 6           6          2 follow_up  Follow-up  response_… Response s… text      
#> # ℹ 12 more variables: descriptive_field <lgl>, required_field <lgl>,
#> #   identifier <lgl>, choice_count <int>, choices <chr>, validation <chr>,
#> #   branching_logic <chr>, event_count <int>, event_names <chr>,
#> #   repeating_status <chr>, field_note <chr>, matrix_group_name <chr>
```

### Metadata and choices

`fields` contains readable summaries. For downstream analysis,
`definitions$fields` and `definitions$project` retain complete
standardized metadata, including additional returned columns.
`definitions$source_columns` distinguishes columns that were returned
from those introduced during normalization; an unavailable source is
different from an available blank value. Field and choice order follow
the metadata, and forms follow their first appearance in that metadata.

`choices` has one row per coded option. Yes/No and True/False options
are supplied implicitly, while calculations are excluded. Codes remain
character values, preserving leading zeros.

For checkbox rows in `codebook$choices` (`choice_type == "checkbox"`),
combine `field_name` and `choice_value` with `___` to obtain the
expected raw export column name:
`paste0(field_name, "___", choice_value)`. For example, `symptoms` with
the choice code `fever` gives `symptoms___fever`. Use the raw code in
`choice_value`, not `choice_label`; `field_name` remains the parent
dictionary field name.

### Events and repeating configuration

Field-level event and repeating summaries describe instrument
configuration across events. A repeating status need not apply to every
event; inspect `event_instruments` and `repeating` for the detailed
context. Classic projects have empty event and event-instrument tables.
Non-applicable configuration responses produce empty tables; permission
and other retrieval failures are reported as errors.

### View and save

[`view_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/view_codebook.md)
presents each available section as an interactive HTML table:

``` r

view_codebook(codebook)
```

See the [`build_codebook()`
reference](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md)
for output details and the available viewing options.

To save a codebook output to HTML, simply use:

``` r

save_codebook(codebook, file = "path.html")
```

## Field relationships

A codebook supplies definitions and event/repeat context for a field
name. Build it once to explore several fields without additional API
calls or record exports.

``` r

relationships <- field_relationship_summary("weight_kg", mock_codebook)
relationships
#> <REDCap field relationships>
#> Field: weight_kg (follow_up)
#> Upstream: 1 item
#> Downstream: 6 direct items, 1 indirect item
#> Reference occurrences across returned chains: 12
#> Action-tag condition: 1; Calculation: 1; Piping: 4; Field embedding: 1; Default value: 3; Unknown relationship type: 1; Branching logic: 1
#> Interpretation issues: 3 related metadata locations (see issues).
#> Project Coverage: project-wide metadata inspection; see coverage for sources and exclusions.
#> Indirect connections are reference chains, not predictions of value or behavior changes.
#> Project interpretation limits are summarized in coverage.
relationships$upstream
#> # A tibble: 1 × 27
#>   item_kind item_name       depth connection path  field_path relationship_types
#>   <chr>     <chr>           <int> <chr>      <lis> <list>     <chr>             
#> 1 field     baseline_weigh…     1 direct     <int> <chr [2]>  Default value (@D…
#> # ℹ 20 more variables: status_note <chr>, reference_chain <chr>,
#> #   field_name <chr>, form_name <chr>, field_type <chr>, field_label <chr>,
#> #   select_choices_or_calculations <chr>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <dbl>, text_validation_max <dbl>, required_field <lgl>,
#> #   branching_logic <chr>, identifier <chr>, field_annotation <chr>,
#> #   field_note <chr>, section_header <chr>, form_label <chr>, …
relationships$downstream
#> # A tibble: 7 × 27
#>   item_kind       item_name depth connection path  field_path relationship_types
#>   <chr>           <chr>     <int> <chr>      <lis> <list>     <chr>             
#> 1 field           bmi           1 direct     <int> <chr [2]>  Calculation       
#> 2 field           visit_su…     1 direct     <int> <chr [2]>  Piping            
#> 3 field           starting…     1 direct     <int> <chr [2]>  Action-tag condit…
#> 4 field           weight_a…     1 direct     <int> <chr [2]>  Default value (@D…
#> 5 field           diagnost…     1 direct     <int> <chr [2]>  Piping            
#> 6 project_setting custom_r…     1 direct     <int> <chr [2]>  Piping            
#> 7 field           weight_r…     2 indirect   <int> <chr [3]>  Calculation; Bran…
#> # ℹ 20 more variables: status_note <chr>, reference_chain <chr>,
#> #   field_name <chr>, form_name <chr>, field_type <chr>, field_label <chr>,
#> #   select_choices_or_calculations <chr>,
#> #   text_validation_type_or_show_slider_number <chr>,
#> #   text_validation_min <dbl>, text_validation_max <dbl>, required_field <lgl>,
#> #   branching_logic <chr>, identifier <chr>, field_annotation <chr>,
#> #   field_note <chr>, section_header <chr>, form_label <chr>, …
```

`relationships$references` preserves expressions and source locations,
while `relationships$issues` retains related interpretation issues and
their evidence. `relationships$coverage` summarizes all project-wide
inspection limits and exclusions, independently of the selected field.
The viewer uses the same Upstream, Downstream, Summary, and Project
Coverage terms and shows every reference by default. The bundled
codebook includes metadata-only examples of indirect paths, annotations,
and interpretation notes around `weight_kg`. Use
`include_diagnostics = TRUE` to include Project Diagnostics, a
project-wide interpretation issue table. Request
`include_inspection_log = TRUE` separately for the full inventory of
inspected locations, including blanks. Use
[`view_field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/view_field_relationship_summary.md)
to explore the report and
[`save_field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/save_field_relationship_summary.md)
to save a standalone HTML viewer. See [Exploring Field
Relationships](https://chop-cgtinformatics.github.io/REDCapExploreR/articles/field-relationships.md)
for details.

## Record status dashboard

[`build_record_status_data()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_record_status_data.md)
summarizes REDCap form completion by record.
[`plot_record_status()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/plot_record_status.md)
converts that table into a ggplot heat map similar to the REDCap Record
Status Dashboard.

``` r

status_data <- build_record_status_data(
  redcap_uri = redcap_uri,
  token = token
)

plot_record_status(status_data)
```

The synthetic `mock_record_status_data` can be plotted directly:

``` r

plot_record_status(mock_record_status_data)
```

![Synthetic REDCap record status dashboard heat
map.](REDCapExploreR_files/figure-html/record-status-mock-1.png)

The legend shows the percentage of instances marked complete among
instances with an exported status. Blank statuses are excluded from that
denominator and appear as missing tiles when no status is available.
Repeating behavior is resolved per event and instrument. Duplicate
display labels include identifiers to keep their tiles distinct,
including across arms.

See the [`plot_record_status()`
reference](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/plot_record_status.md)
for compact mode and display customization.

## Quality report

[`build_quality_report()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_quality_report.md)
retrieves records and project structure, runs general data quality
checks, and returns findings, summaries, and standardized metadata.

``` r

report <- build_quality_report(
  redcap_uri = redcap_uri,
  token = token
)

report
report$summaries$project
report$findings
```

Use `mock_quality_report` to explore the report structure and example
findings:

``` r

mock_quality_report
#> <REDCap quality report>
#> Records: 3
#> Fields: 11
#> Findings: 7
#> Checks: missingness, metadata, outliers, operational, consistency
#> Missingness assessed: 24 eligible field-row values

mock_quality_report$findings |>
  dplyr::select(
    finding_id,
    check,
    issue,
    severity,
    record_id,
    field_name
  )
#> # A tibble: 7 × 6
#>   finding_id check       issue                    severity record_id field_name 
#>        <int> <chr>       <chr>                    <chr>    <chr>     <chr>      
#> 1          1 metadata    high_risk_free_text      info     NA        visit_notes
#> 2          2 outliers    outside_validation_range warning  C002      age        
#> 3          3 outliers    future_date              info     C002      enrollment…
#> 4          4 operational incomplete_form_status   info     C0003     demographi…
#> 5          5 operational incomplete_form_status   info     C002      follow_up_…
#> 6          6 operational incomplete_form_status   info     C002      adverse_ev…
#> 7          7 operational incomplete_form_status   info     C0003     adverse_ev…
```

See the [quality report
article](https://chop-cgtinformatics.github.io/REDCapExploreR/articles/quality-report.md)
for check definitions, output structure, interpretation guidance, and
core assumptions.

## Advanced project data access

[`pull_redcap_project()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/pull_redcap_project.md)
retrieves the record and structural metadata tables used by the
quality-report workflow. Use it when a custom analysis needs the
underlying API responses directly.

``` r

project <- pull_redcap_project(
  redcap_uri = redcap_uri,
  token = token
)

names(project)
```

`mock_redcap_project` has the same top-level structure:

``` r

names(mock_redcap_project)
#> [1] "data"                  "metadata"              "events"               
#> [4] "event_instruments"     "instruments"           "repeating_instruments"
#> [7] "project_info"
```

## Next steps

- Use the [quality report
  article](https://chop-cgtinformatics.github.io/REDCapExploreR/articles/quality-report.md)
  for a deeper review of quality checks and report interpretation.
- Browse the [function
  reference](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/index.md)
  for complete arguments and return values.
- Explore `mock_redcap_project`, `mock_record_status_data`,
  `mock_codebook`, and `mock_quality_report` when REDCap credentials are
  not available.
