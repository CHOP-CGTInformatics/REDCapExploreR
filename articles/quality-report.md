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

## How to Build a Report

### Generate a report object

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

### Set heuristic thresholds

Two arguments control when heuristic findings are produced. They do not
change the underlying REDCap data or missing-rate calculations, but they
can change findings and the associated `finding_count` summaries.

`sparse_threshold` is a proportion greater than `0` and less than or
equal to `1`. For each field, the report calculates
`missing_count / record_count` across rows eligible for missingness
assessment. An `unexpected_sparse_field` finding is generated when that
missing rate is greater than or equal to `sparse_threshold`. The default
`0.95` therefore flags a field when at least 95% of its assessed values
are missing. For example, a field with 19 missing values among 20
applicable rows has a missing rate of `0.95` and is flagged at the
default threshold. Raising the threshold produces fewer sparse-field
findings; lowering it makes the check more sensitive.

`outlier_iqr_multiplier` is a positive number used only by the
`numeric_iqr_outlier` check. For each exported numeric, non-choice field
other than the record identifier, the report calculates the first
quartile (Q1), third quartile (Q3), and `IQR = Q3 - Q1`, then defines
the heuristic range as:

``` text
lower bound = Q1 - outlier_iqr_multiplier * IQR
upper bound = Q3 + outlier_iqr_multiplier * IQR
```

Values strictly outside that range are flagged. The default multiplier
of `3` uses wider bounds than the conventional `1.5 * IQR` rule and is
therefore less sensitive to moderate extremes. Smaller multipliers
produce more findings; larger multipliers produce fewer. The check
requires at least four observed values and a nonzero IQR, excludes
choice fields and the record identifier, and does not affect REDCap
validation-range or future-date findings.

### Control progress output

`progress = "auto"` is the default. It enables normally throttled
progress updates in interactive sessions, such as an R console, and
suppresses them in noninteractive scripts and most automated jobs.
`progress = "show"` enables progress in all sessions and forces every
report step to render. Use `progress = "none"` when no progress output
should be emitted.

### Start with the report overview

Printing the report gives a short project-level overview. The object
itself is a list with `findings`, `summaries`, and `metadata` elements.

``` r

mock_quality_report
#> <REDCap quality report>
#> Records: 3
#> Fields: 11
#> Findings: 7

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
#> # A tibble: 4 × 4
#>   check       issue                    severity     n
#>   <chr>       <chr>                    <chr>    <int>
#> 1 operational incomplete_form_status   info         4
#> 2 metadata    high_risk_free_text      info         1
#> 3 outliers    future_date              info         1
#> 4 outliers    outside_validation_range warning      1
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
#> # A tibble: 6 × 10
#>   finding_id severity issue               record_id event_name repeat_instrument
#>        <int> <chr>    <chr>               <chr>     <chr>      <chr>            
#> 1          2 warning  outside_validation… C002      baseline_… NA               
#> 2          3 info     future_date         C002      baseline_… NA               
#> 3          4 info     incomplete_form_st… C0003     baseline_… NA               
#> 4          5 info     incomplete_form_st… C002      month_1_a… NA               
#> 5          6 info     incomplete_form_st… C002      month_1_a… adverse_events   
#> 6          7 info     incomplete_form_st… C0003     month_1_a… adverse_events   
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
#> # A tibble: 4 × 8
#>   finding_id severity issue          form_name field_name value expected message
#>        <int> <chr>    <chr>          <chr>     <chr>      <chr> <chr>    <chr>  
#> 1          2 warning  outside_valid… demograp… age        999   Between… Field …
#> 2          3 info     future_date    demograp… enrollmen… 2999… On or b… Date f…
#> 3          5 info     incomplete_fo… follow_up follow_up… Inco… Complete Comple…
#> 4          6 info     incomplete_fo… adverse_… adverse_e… Unve… Complete Comple…
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
#> # A tibble: 3 × 7
#>   finding_id issue    record_id field_name field_label field_type required_field
#>        <int> <chr>    <chr>     <chr>      <chr>       <chr>      <lgl>         
#> 1          1 high_ri… NA        visit_not… Visit notes notes      FALSE         
#> 2          2 outside… C002      age        Age at enr… text       TRUE          
#> 3          3 future_… C002      enrollmen… Enrollment… text       TRUE
```

## Missingness

Missingness is evaluated in two stages. The report first determines
whether a field should be assessed on a particular exported row. It then
determines whether that field has an observed value. This prevents
structural blanks from events, repeating instruments, branching logic,
and unfinished forms from being treated as missing data.

### Form status determines which rows are assessed

A field is eligible for missingness assessment only when its raw REDCap
form status column, `<form_name>_complete`, is Unverified (`1`) or
Complete (`2`). Rows with an Incomplete (`0`), blank, or unavailable
form status do not contribute to field `record_count`, `missing_count`,
or `missing_rate`.

Non-Complete statuses, including Incomplete, Unverified, and blank
values, are still reviewable through the `incomplete_form_status`
operational finding. The distinction is intentional: Unverified forms
are treated as sufficiently assessable but have not been marked
Complete, while Incomplete forms can contain expected blanks that should
not inflate missingness findings. If a form has no exported completion
status column, its fields have no assessable rows and their missing
rates are `NA`.

### Per-field missingness evaluation

Among rows with an assessable form status, applicability also accounts
for:

1.  The event-to-form mapping in longitudinal projects.
2.  Repeating event and instrument configuration.
3.  The repeating instrument associated with each exported row.
4.  Supported branching logic that determines whether the field is
    shown.

For applicable rows, `NA` and blank or whitespace-only values are
missing. Each field summary reports:

- `record_count`: applicable rows assessed for the field.
- `missing_count`: applicable rows with a missing value.
- `observed_count`: applicable rows with an observed value.
- `missing_rate`: `missing_count / record_count`, or `NA` when no rows
  are applicable.

A missing required field produces a record-level
`required_field_missing` finding. Any field, whether required or
optional, can produce a field-level `unexpected_sparse_field` finding
when its missing rate meets `sparse_threshold`.

### Checkbox fields

REDCap exports a checkbox field as one `0`/`1` column per choice. The
report evaluates missingness at the parent-field level rather than
treating every unchecked choice as a missing value:

- One or more checked (`1`) choices make the parent checkbox field
  observed.
- All choices explicitly unchecked (`0`) are also observed. This
  represents a meaningful no-selection state rather than a missing
  response.
- The parent field is missing only when every exported choice value is
  `NA` or blank on an otherwise applicable row.
- A checked explicit option labeled “None”, “No”, “Not applicable”,
  “N/A”, “NA”, or “None of the above” is an observed response. Selecting
  one of these with another option can separately produce a
  `checkbox_none_with_other` consistency finding.

The form-status rule is applied first. Consequently, checkbox exports on
an Incomplete form are ignored for missingness. On an Unverified or
Complete form, an explicit all-zero state does not contribute to
missingness, while an all-missing set of choice exports can contribute
to sparse-field summaries and, for a required checkbox field, a
`required_field_missing` finding.

Both checkbox consistency findings use only applicable rows from
Unverified or Complete forms. When no options are selected anywhere for
a checkbox field, the report produces one field-level
`checkbox_no_values_selected` informational finding. It is a prompt to
confirm that the no-selection state is intentional, not a missingness
finding.

## Metadata-Defined Validation

Choice and text validation findings compare exported raw values with
rules configured in REDCap metadata. Unlike missingness, these checks
inspect nonblank values on structurally applicable rows regardless of
whether a form is Incomplete, Unverified, or Complete. Event-to-form and
repeating-instrument mappings are respected. Branching logic does not
suppress these findings: a stored nonblank value that violates its
metadata remains reviewable even when the field is currently hidden.

The records pull disables automatic column type guessing so raw choice
codes such as `0` and `1` remain character values rather than being
converted to logical `FALSE` and `TRUE`. Only empty CSV values are
imported as missing; literal codes such as `"NA"` are preserved for
comparison with metadata.

### Choice values

`invalid_choice_value` identifies raw codes that are not among a field’s
metadata choices for radio, dropdown, Yes/No, and True/False fields.
Yes/No and True/False use their implicit REDCap `0`/`1` definitions.

Checkbox fields are not assessed by `invalid_choice_value`. Their
metadata codes identify the separate exported option columns, while each
option column contains a `0` or `1` selection state. Checkbox review
remains available through `checkbox_no_values_selected` and
`checkbox_none_with_other`.

Fields without an explicit radio, dropdown, or checkbox definition
cannot be compared with allowed codes. They produce
`missing_choice_definition` under the metadata check instead of an
invalid-choice finding.

### Text validation formats and bounds

`invalid_validation_format` applies only when all of the following are
true:

1.  The metadata `field_type` is `text`.
2.  `text_validation_type_or_show_slider_number` contains a supported
    REDCap validation type.
3.  The exported value is nonblank on a structurally applicable row.

The report does not infer validation from an R column class, field name,
question text, or observed values. Text fields without configured
validation and unsupported validation types are not assessed.

Supported metadata validation families are:

- `integer`; `number`; decimal-place variants from `number_1dp` through
  `number_4dp`; and their `*_comma_decimal` variants.
- `date_dmy`, `date_mdy`, and `date_ymd`.
- DMY, MDY, and YMD `datetime_*` types, with or without seconds.
- `time`, `time_hh_mm_ss`, and `time_mm_ss`.
- `email`, `phone`, and `phone_australia`.

The DMY, MDY, and YMD suffix controls REDCap’s data-entry display, but
REDCap exports stored date and datetime values through the API in
canonical YMD order. The report therefore validates all date types as
`YYYY-MM-DD` and datetime types as `YYYY-MM-DD HH:MM` or
`YYYY-MM-DD HH:MM:SS`, depending on whether the validation includes
seconds.

Parsing remains strict for impossible calendar or clock values, decimal
separators, configured decimal precision, and scientific notation.
REDCap number validations accept decimal values with or without a
leading zero, such as `0.850` and `.850`. Email checks assess structure
rather than deliverability. Phone checks allow common punctuation and
assess North American or Australian digit structure; they do not verify
that a number is assigned.

When supplied, `text_validation_min` and `text_validation_max` produce
`outside_validation_range` findings for validly formatted numeric, date,
datetime, and time values outside those user-configured bounds. Either
bound can be used alone. A malformed value produces
`invalid_validation_format` and is excluded from range and future-date
checks to avoid duplicate or misleading findings.

## Quality Report Structure

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
| Metadata | `high_risk_free_text` | A text or notes field name or label contains terms associated with notes, comments, other details, or similar free text. | Info, field |
| Outliers | `outside_validation_range` | A validly formatted numeric, date, datetime, or time value is outside `text_validation_min` or `text_validation_max`. | Warning, `record_field` |
| Outliers | `numeric_iqr_outlier` | A numeric, non-choice field falls outside the configured IQR heuristic. At least four observed values and a nonzero IQR are required. | Info, `record_field` |
| Outliers | `future_date` | A validly formatted date or datetime value occurs after the report date. | Info, `record_field` |
| Operational | `incomplete_form_status` | An applicable form status is missing or is not Complete. | Info, `record_field` |
| Consistency | `invalid_choice_value` | A raw radio, dropdown, Yes/No, or True/False code is absent from metadata choices. | Warning, `record_field` |
| Consistency | `invalid_validation_format` | A text value does not satisfy its configured `text_validation_type_or_show_slider_number` rule. | Warning, `record_field` |
| Consistency | `checkbox_no_values_selected` | No options are selected for a checkbox field across applicable Unverified or Complete forms. | Info, field |
| Consistency | `checkbox_none_with_other` | A checkbox option labeled None, No, Not applicable, N/A, NA, or None of the above is selected with another option. | Warning, `record_field` |

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
#> 1 C002                        0             4
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

### Branching logic support is intentionally bounded

The branching evaluator supports ordinary field comparisons, checkbox
choice references, parentheses, and `and`/`or` combinations. A valid
expression that evaluates to `NA` is treated as not shown for that row.

REDCap functions, smart variables, and other expressions outside the
supported subset are treated as applicable rather than used to hide a
potentially required value. The report does not produce findings for
apparently missing branching references because event-qualified fields,
cross-form fields, and smart variables cannot be classified reliably
from bracketed tokens alone.

### Validation and outlier checks have different sources

Format and range findings enforce user-created `text_validation_*`
metadata for text fields. The validation type determines how values and
optional bounds are parsed. Unsupported or absent validation types do
not produce format findings, and malformed values are not reported as
range or future-date findings.

IQR findings are separate distribution-based heuristics. They require at
least four observed values and a nonzero IQR, and their usefulness
depends on the field’s distribution and selected multiplier.

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
