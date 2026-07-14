# Understanding REDCap Quality Reports

``` r

library(REDCapExploreR)
```

[`build_quality_report()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_quality_report.md)
retrieves REDCap records and project structure, applies general data
quality checks, and organizes the results for review. The report is
intended to help data managers and analysts identify records, fields,
and metadata that warrant attention. It does not modify the REDCap
project.

This article explains how to build and navigate a report. All evaluated
examples use `mock_quality_report`, a synthetic report included with the
package, so they do not require REDCap credentials.

## How to Use

### Build a report

Store the REDCap API URI and token outside version-controlled code, such
as in environment variables, and pass them to
[`build_quality_report()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_quality_report.md):

``` r

report <- build_quality_report(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)
```

`checks` controls which findings are generated. Summaries and
interpreted metadata are still returned, while `finding_count` values in
the project and record summaries reflect the selected check groups.
`mock_quality_report` was built with all default check groups and
thresholds.

The default call runs all five check groups: `missingness`, `metadata`,
`outliers`, `operational`, and `consistency`. Use `checks` to run a
focused review:

``` r

report <- build_quality_report(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN"),
  checks = c("missingness", "metadata"),
  progress = "none"
)
```

Two arguments control heuristic thresholds:

- `sparse_threshold` is the applicable-row missing rate at which a field
  is reported as unexpectedly sparse. It defaults to `0.95`.
- `outlier_iqr_multiplier` controls the distance from the first and
  third quartiles used by the IQR outlier check. It defaults to `3`.

`progress = "auto"` displays progress in interactive sessions. Use
`"show"` to force it or `"none"` to suppress it.

### Start with the report overview

Printing the report gives a short project-level overview. The object
itself is a list with `findings`, `summaries`, and `metadata` elements.

``` r

mock_quality_report
#> <REDCap quality report>
#> Records: 3
#> Fields: 11
#> Findings: 8

names(mock_quality_report)
#> [1] "findings"  "summaries" "metadata"
names(mock_quality_report$summaries)
#> [1] "project" "forms"   "fields"  "records" "events"
names(mock_quality_report$metadata)
#>  [1] "fields"                "forms"                 "events"               
#>  [4] "event_instruments"     "instruments"           "repeating_instruments"
#>  [7] "choices"               "validation"            "branching"            
#> [10] "record_id_field"       "source"
```

Begin with the project summary and a count of findings by check and
issue. This shows the size and structure of the project alongside the
types of review items that were found.

``` r

mock_quality_report$summaries$project
#> # A tibble: 1 × 8
#>   record_count raw_row_count field_count form_count event_count
#>          <int>         <int>       <int>      <int>       <int>
#> 1            3            10          11          3           2
#> # ℹ 3 more variables: repeating_enabled <lgl>, missing_rate <dbl>,
#> #   finding_count <int>

mock_quality_report$findings |>
  dplyr::count(check, issue, severity, sort = TRUE)
#> # A tibble: 5 × 4
#>   check       issue                    severity     n
#>   <chr>       <chr>                    <chr>    <int>
#> 1 operational incomplete_form_status   info         4
#> 2 consistency checkbox_none_with_other warning      1
#> 3 metadata    high_risk_free_text      info         1
#> 4 outliers    future_date              info         1
#> 5 outliers    outside_validation_range warning      1
```

### Review actionable findings

Use `scope` to distinguish findings tied to a specific record and field
from field-level metadata findings. The context columns remain available
for longitudinal and repeating projects.

``` r

record_findings <- mock_quality_report$findings |>
  dplyr::filter(scope == "record_field") |>
  dplyr::select(
    finding_id,
    severity,
    issue,
    record_id,
    event_name,
    repeat_instrument,
    repeat_instance,
    field_name,
    value,
    expected
  )

record_findings
#> # A tibble: 7 × 10
#>   finding_id severity issue               record_id event_name repeat_instrument
#>        <int> <chr>    <chr>               <chr>     <chr>      <chr>            
#> 1          2 warning  outside_validation… C002      baseline_… NA               
#> 2          3 info     future_date         C002      baseline_… NA               
#> 3          4 info     incomplete_form_st… C0003     baseline_… NA               
#> 4          5 info     incomplete_form_st… C002      month_1_a… NA               
#> 5          6 info     incomplete_form_st… C002      month_1_a… adverse_events   
#> 6          7 info     incomplete_form_st… C0003     month_1_a… adverse_events   
#> 7          8 warning  checkbox_none_with… C002      month_1_a… NA               
#> # ℹ 4 more variables: repeat_instance <chr>, field_name <chr>, value <chr>,
#> #   expected <chr>
```

Filter by record, issue, form, or severity to create a review queue.
Severity is a package-provided prioritization aid, not a clinical or
regulatory classification.

``` r

mock_quality_report$findings |>
  dplyr::filter(record_id == "C002") |>
  dplyr::select(
    finding_id,
    severity,
    issue,
    form_name,
    field_name,
    value,
    expected,
    message
  )
#> # A tibble: 5 × 8
#>   finding_id severity issue          form_name field_name value expected message
#>        <int> <chr>    <chr>          <chr>     <chr>      <chr> <chr>    <chr>  
#> 1          2 warning  outside_valid… demograp… age        999   Between… Field …
#> 2          3 info     future_date    demograp… enrollmen… 2999… On or b… Date f…
#> 3          5 info     incomplete_fo… follow_up follow_up… Inco… Complete Comple…
#> 4          6 info     incomplete_fo… adverse_… adverse_e… Unve… Complete Comple…
#> 5          8 warning  checkbox_none… follow_up symptoms   symp… None op… Checkb…
```

Join findings to interpreted field metadata when labels and field types
are useful during review:

``` r

field_reference <- mock_quality_report$metadata$fields |>
  dplyr::select(field_name, field_label, field_type, required_field)

mock_quality_report$findings |>
  dplyr::filter(!is.na(field_name)) |>
  dplyr::inner_join(field_reference, by = "field_name") |>
  dplyr::select(
    finding_id,
    issue,
    record_id,
    field_name,
    field_label,
    field_type,
    required_field
  )
#> # A tibble: 4 × 7
#>   finding_id issue    record_id field_name field_label field_type required_field
#>        <int> <chr>    <chr>     <chr>      <chr>       <chr>      <lgl>         
#> 1          1 high_ri… NA        visit_not… Visit notes notes      FALSE         
#> 2          2 outside… C002      age        Age at enr… text       TRUE          
#> 3          3 future_… C002      enrollmen… Enrollment… text       TRUE          
#> 4          8 checkbo… C002      symptoms   Symptoms    checkbox   FALSE
```

## Key Elements

### Findings

`findings` is a standardized tibble with one row per detected issue. A
report with no findings returns an empty tibble with the same columns.

| Column | Meaning |
|----|----|
| `finding_id` | Sequential identifier assigned within the report. |
| `check` | Check group that produced the finding. |
| `issue` | Stable issue name suitable for filtering and counting. |
| `severity` | Package-assigned `warning` or `info` review priority. |
| `scope` | `record_field` for record-level findings or `field` for metadata and aggregate findings. |
| `record_id` | REDCap record identifier when the finding is record-specific. |
| `form_name` | REDCap instrument associated with the finding. |
| `event_name` | Unique REDCap event name when available. |
| `repeat_instrument` | Repeating instrument name when available. |
| `repeat_instance` | Repeating instance number when available. |
| `field_name` | REDCap field associated with the finding. |
| `value` | Observed value or aggregate value that triggered the finding. |
| `expected` | Concise description of the expected state or range. |
| `message` | Human-readable explanation of the finding. |

The following checks are currently performed.

| Check | Issue | What it identifies | Severity and scope |
|----|----|----|----|
| Missingness | `required_field_missing` | A required field is blank on a row where the form and branching logic are applicable and completion status permits missingness assessment. | Warning, `record_field` |
| Missingness | `unexpected_sparse_field` | A field’s missing rate is at or above `sparse_threshold` among its applicable rows. | Info, field |
| Metadata | `duplicate_field_label` | The same nonblank field label is used by multiple field names. | Info, field |
| Metadata | `missing_field_label` | A field has no label. | Info, field |
| Metadata | `missing_choice_definition` | A radio, dropdown, or checkbox field has no explicit choice definition. | Warning, field |
| Metadata | `orphaned_branching_reference` | Branching logic references a field not present in project metadata. | Warning, field |
| Metadata | `high_risk_free_text` | A text or notes field name or label contains terms associated with notes, comments, other details, or similar free text. | Info, field |
| Outliers | `outside_validation_range` | A numeric value is below or above a REDCap metadata validation bound. | Warning, `record_field` |
| Outliers | `numeric_iqr_outlier` | A numeric, non-choice field falls outside the configured IQR heuristic. At least four observed values and a nonzero IQR are required. | Info, `record_field` |
| Outliers | `future_date` | A field with date validation contains a `YYYY-MM-DD` value after the report date. | Info, `record_field` |
| Operational | `incomplete_form_status` | An applicable form status is missing or is not Complete. | Info, `record_field` |
| Consistency | `checkbox_none_with_other` | A checkbox option labeled as none, no, or not applicable is selected with another option. | Warning, `record_field` |

Choice fields are excluded from IQR outlier detection even when their
codes are numeric. Yes/No and True/False fields have implicit REDCap
choices, so they do not require an explicit choice definition.

### Summaries

`summaries` provides five views of the normalized project and report
findings.

#### Project

`summaries$project` is a one-row overview:

- `record_count`: number of distinct record identifiers.
- `raw_row_count`: number of rows returned by the records API, including
  event and repeating rows.
- `field_count`, `form_count`, and `event_count`: project structure
  counts.
- `repeating_enabled`: whether repeating configuration was returned by
  REDCap.
- `missing_rate`: weighted missing fraction across applicable field-row
  cells.
- `finding_count`: total number of findings from the selected check
  groups.

``` r

mock_quality_report$summaries$project
#> # A tibble: 1 × 8
#>   record_count raw_row_count field_count form_count event_count
#>          <int>         <int>       <int>      <int>       <int>
#> 1            3            10          11          3           2
#> # ℹ 3 more variables: repeating_enabled <lgl>, missing_rate <dbl>,
#> #   finding_count <int>
```

#### Forms and fields

`summaries$forms` reports field counts, required-field counts, and
weighted missing rates by form. `summaries$fields` gives the most
detailed missingness profile:

- `record_count` is the number of applicable rows assessed for the
  field. It is not necessarily the number of distinct records because
  longitudinal and repeating projects can contribute multiple rows per
  record.
- `missing_count` and `observed_count` partition applicable rows.
- `missing_rate` is `missing_count / record_count`, or `NA` when no rows
  are applicable.
- `distinct_count` counts unique nonmissing values.

``` r

mock_quality_report$summaries$forms
#> # A tibble: 3 × 4
#>   form_name      field_count required_field_count missing_rate
#>   <chr>                <int>                <int>        <dbl>
#> 1 adverse_events           3                    2            0
#> 2 demographics             4                    3            0
#> 3 follow_up                4                    1            0

mock_quality_report$summaries$fields |>
  dplyr::arrange(dplyr::desc(missing_rate), field_name) |>
  dplyr::select(
    field_name,
    form_name,
    required_field,
    record_count,
    missing_count,
    missing_rate,
    distinct_count
  )
#> # A tibble: 11 × 7
#>    field_name   form_name required_field record_count missing_count missing_rate
#>    <chr>        <chr>     <lgl>                 <int>         <int>        <dbl>
#>  1 ae_grade     adverse_… TRUE                      4             0            0
#>  2 ae_related   adverse_… FALSE                     4             0            0
#>  3 ae_term      adverse_… TRUE                      4             0            0
#>  4 age          demograp… TRUE                      2             0            0
#>  5 enrollment_… demograp… TRUE                      2             0            0
#>  6 record_id    demograp… TRUE                      2             0            0
#>  7 response_sc… follow_up FALSE                     1             0            0
#>  8 sex          demograp… FALSE                     2             0            0
#>  9 visit_date   follow_up TRUE                      1             0            0
#> 10 visit_notes  follow_up FALSE                     1             0            0
#> 11 symptoms     follow_up FALSE                     0             0           NA
#> # ℹ 1 more variable: distinct_count <int>
```

Project and form missing rates are weighted across applicable field-row
cells. They are not simple averages of field-level percentages.

#### Records and events

`summaries$records` contains one row per record. `missing_field_count`
sums missing applicable field values across that record’s events and
repeat instances, while `finding_count` counts record-specific findings.
Field-level metadata findings do not belong to an individual record.

`summaries$events` counts raw API rows by unique event name. It is empty
for nonlongitudinal projects.

``` r

mock_quality_report$summaries$records |>
  dplyr::arrange(dplyr::desc(finding_count), record_id)
#> # A tibble: 3 × 3
#>   record_id missing_field_count finding_count
#>   <chr>                   <dbl>         <int>
#> 1 C002                        0             5
#> 2 C0003                       0             2
#> 3 C001                        0             0

mock_quality_report$summaries$events
#> # A tibble: 2 × 2
#>   event_name     row_count
#>   <chr>              <int>
#> 1 baseline_arm_1         3
#> 2 month_1_arm_1          7
```

### Metadata

`metadata` preserves and interprets project structure used by the
checks.

| Element | Contents |
|----|----|
| `fields` | Standardized REDCap field metadata used by the report. |
| `forms` | One row per form with field, required-field, and choice-field counts. |
| `events` | Standardized longitudinal event metadata. |
| `event_instruments` | Standardized event-to-form mapping. |
| `instruments` | Instrument names and labels returned by REDCap. |
| `repeating_instruments` | Repeating event and instrument configuration returned by REDCap, with normalized join columns. |
| `choices` | Parsed explicit choices and implicit Yes/No and True/False choices. |
| `validation` | Fields with validation types or minimum/maximum bounds. |
| `branching` | Fields with branching logic and parsed referenced field names. |
| `record_id_field` | Field name used as the project record identifier. |
| `source` | Source of the normalized project; API reports use `"api"`. |

These tables are useful for interpreting findings and for project-level
review that is not represented by a finding. For example, inspect
repeating structure and validation rules directly:

``` r

mock_quality_report$metadata$repeating_instruments
#> # A tibble: 1 × 6
#>   event_name    arm_num unique_event_name form_name      custom_form_label
#>   <chr>           <int> <chr>             <chr>          <chr>            
#> 1 month_1_arm_1       1 month_1_arm_1     adverse_events NA               
#> # ℹ 1 more variable: redcap_event_name <chr>

mock_quality_report$metadata$validation
#> # A tibble: 5 × 5
#>   field_name      form_name      text_validation_type_or_s…¹ text_validation_min
#>   <chr>           <chr>          <chr>                                     <dbl>
#> 1 age             demographics   integer                                       0
#> 2 enrollment_date demographics   date_ymd                                     NA
#> 3 visit_date      follow_up      date_ymd                                     NA
#> 4 response_score  follow_up      integer                                       0
#> 5 ae_grade        adverse_events integer                                       1
#> # ℹ abbreviated name: ¹​text_validation_type_or_show_slider_number
#> # ℹ 1 more variable: text_validation_max <dbl>

mock_quality_report$metadata$branching
#> # A tibble: 1 × 4
#>   field_name form_name branching_logic         referenced_fields
#>   <chr>      <chr>     <chr>                   <chr>            
#> 1 symptoms   follow_up [response_score] < '10' response_score
```

## Core Assumptions

### Findings are review prompts

The checks are broad heuristics intended to identify items worth
reviewing. They do not establish clinical correctness, protocol
compliance, or regulatory compliance. An `info` finding can still be
important, and a report with no findings does not prove that the data
are correct.

`finding_id` is assigned when a report is built and should not be
treated as a persistent identifier across runs. Use issue and REDCap
context columns when tracking findings externally.

### Missingness is assessed only where a field is applicable

`NA` and blank or whitespace-only values are treated as missing. For
checkbox fields, one or more checked options make the field observed; a
set of unchecked exports is treated as missing when the field is
assessed.

Applicability combines several pieces of project structure:

1.  Longitudinal forms are limited to events in the event-to-form
    mapping.
2.  Repeating instruments are identified from REDCap repeating
    configuration, not inferred solely from rows currently present in
    the data export.
3.  Repeating rows are matched to their configured instrument so
    structural blanks from other repeating instruments are not counted
    as missing.
4.  Branching logic limits assessment to rows where the field should be
    shown.
5.  Missingness is assessed only when `<form_name>_complete` is
    Unverified (`1`) or Complete (`2`). Incomplete (`0`) forms are
    handled by the operational check instead.

This design avoids treating structural blanks as missing data. It also
means a field can have `record_count = 0` and `missing_rate = NA` when
no exported rows are eligible for assessment.

### Branching logic support is intentionally bounded

The branching evaluator supports ordinary field comparisons, checkbox
choice references, parentheses, and `and`/`or` combinations. A valid
expression that evaluates to `NA` is treated as not shown for that row.

REDCap functions, smart variables, and other expressions outside the
supported subset are treated as applicable rather than used to hide a
potentially required value. The metadata check separately reports
references to fields that are absent from project metadata. Complex
branching findings should therefore be reviewed against REDCap itself.

### Outlier checks depend on metadata and sample size

Validation-range findings depend on numeric minimum and maximum values
defined in REDCap metadata. IQR findings are distribution-based and
require at least four observed values and a nonzero IQR. Their
usefulness depends on the field’s distribution and the selected
multiplier.

Future-date checks apply only to fields whose REDCap validation type
contains `date`, and values are interpreted in `YYYY-MM-DD` form.
Invalid date strings are not reported as future dates.

### Completion status drives two different questions

Missingness asks whether expected values are present on forms
sufficiently complete to assess. The operational check asks whether
accessible forms have a Complete status. For that reason, a form marked
Incomplete can produce an operational finding without contributing
required-field missingness findings.

### Counts reflect REDCap’s row structure

A REDCap record can produce multiple API rows because of events and
repeating instances. Project `record_count` counts distinct records,
`raw_row_count` counts exported rows, field `record_count` counts
applicable rows, and event `row_count` counts exported rows per event.
Choose the measure that matches the question being asked.

### The report depends on current REDCap structure

Descriptive fields are removed before analysis because they do not store
record values. Structural metadata retrieval failures are reported as
errors rather than silently converted to empty project structure.
Results reflect the API records and metadata available when the report
is built, including the token’s project access and export configuration.
