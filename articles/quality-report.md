# Understanding REDCap Quality Reports

``` r

library(REDCapExploreR)
```

[`build_quality_report()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_quality_report.md)
retrieves REDCap records and project structure, applies general data
quality checks, and organizes the results for review. It helps data
managers and analysts identify records, fields, and metadata that
warrant attention without modifying the REDCap project.

This article explains how to build, review, and interpret a quality
report. All evaluated examples use `mock_quality_report`, a synthetic
report included with the package, so they do not require REDCap
credentials.

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

### Choose checks

The default call runs all five check groups: `missingness`, `metadata`,
`outliers`, `operational`, and `consistency`. Use `checks` for a focused
review:

``` r

report <- build_quality_report(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN"),
  checks = c("missingness", "metadata")
)
```

Summaries and standardized metadata are always returned. Project- and
record-level `finding_count` values reflect only the selected check
groups. `mock_quality_report` was built with all default checks and
thresholds.

### Set heuristic thresholds

Two arguments control heuristic findings. They do not change the
underlying REDCap data or missing-rate calculations.

| Argument | Default | Finding affected | Lower values | Higher values |
|----|---:|----|----|----|
| `sparse_threshold` | `0.95` | `unexpected_sparse_field` | Flag more fields | Flag fewer fields |
| `outlier_iqr_multiplier` | `3` | `numeric_iqr_outlier` | Flag more values | Flag fewer values |

For each field, `sparse_threshold` is compared with
`missing_count / record_count` across rows eligible for missingness
assessment. A field is flagged when its missing rate is greater than or
equal to the threshold. At the default `0.95`, a field with 19 missing
values among 20 applicable rows is flagged.

For each numeric, non-choice field other than the record identifier,
`outlier_iqr_multiplier` defines the heuristic range:

``` text
lower bound = Q1 - outlier_iqr_multiplier * IQR
upper bound = Q3 + outlier_iqr_multiplier * IQR
```

Values strictly outside the range are flagged. The default multiplier of
`3` uses wider bounds than the conventional `1.5 * IQR` rule and is less
sensitive to moderate extremes. The check requires at least four
observed values and a nonzero IQR. It does not affect metadata-defined
validation ranges or future-date findings.

## How to Review a Report

A practical review usually follows four steps:

1.  Inspect the project overview.
2.  Count findings by check, issue, and severity.
3.  Filter record-field findings into a review queue.
4.  Join field metadata when labels or field types add context.

### Start with the report overview

Printing the report gives a short project-level overview. The object is
a list with `findings`, `summaries`, and `metadata` elements.

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

Begin with the project summary and a count of findings. This shows
project size and structure alongside the types of review items found.

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

Use `scope` to separate record-field findings from field-level metadata
and aggregate findings. Event and repeat columns preserve context for
longitudinal and repeating projects.

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

Join findings to field metadata when labels and field types are useful
during review:

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
| `scope` | `record_field` for record-field findings or `field` for metadata and aggregate findings. |
| `record_id` | REDCap record identifier when the finding is record-specific. |
| `form_name` | REDCap instrument associated with the finding. |
| `event_name` | Unique REDCap event name when available. |
| `repeat_instrument` | Repeating instrument name when available. |
| `repeat_instance` | Repeating instance number when available. |
| `field_name` | REDCap field associated with the finding. |
| `value` | Observed or aggregate value that triggered the finding. |
| `expected` | Concise description of the expected state or range. |
| `message` | Human-readable explanation of the finding. |

The current findings are grouped below by check.

#### Missingness

These checks describe absent data only where a field is eligible for
assessment. They identify required values that are missing on specific
rows and fields whose eligible values are almost entirely missing.

| Issue | What it identifies | Severity and scope |
|----|----|----|
| `required_field_missing` | A required field is blank where form status, project structure, and supported branching logic permit assessment. | Warning, `record_field` |
| `unexpected_sparse_field` | A field’s missing rate meets or exceeds `sparse_threshold` among applicable rows. | Info, `field` |

#### Metadata

Metadata checks inspect project design rather than the validity of
individual record values. They highlight labels, choice definitions, and
free-text fields that may need review by a project designer or data
manager.

| Issue | What it identifies | Severity and scope |
|----|----|----|
| `duplicate_field_label` | The same nonblank label is used by multiple field names. | Info, `field` |
| `missing_field_label` | A field has no label. | Info, `field` |
| `missing_choice_definition` | A radio, dropdown, or checkbox field has no explicit choice definition. | Warning, `field` |
| `high_risk_free_text` | A text or notes field name or label contains terms associated with notes, comments, other details, or similar free text. | Info, `field` |

Yes/No and True/False fields use implicit REDCap choices, so they do not
require explicit choice definitions.

#### Outliers

Outlier checks combine rules configured in REDCap with general review
heuristics. They identify values outside metadata bounds, unusually
distant numeric values, and dates after the report is built.

| Issue | What it identifies | Severity and scope |
|----|----|----|
| `outside_validation_range` | A valid numeric, date, datetime, or time value is outside `text_validation_min` or `text_validation_max`. | Warning, `record_field` |
| `numeric_iqr_outlier` | A numeric, non-choice value is outside the configured IQR heuristic. | Info, `record_field` |
| `future_date` | A valid date or datetime has a calendar date after the system date captured when the report is built. | Info, `record_field` |

Choice fields are excluded from IQR outlier detection even when their
stored codes are numeric, because those codes represent categories
rather than quantities.

#### Operational

Operational checks review REDCap workflow state rather than field
values. The current check identifies applicable forms that have not been
marked Complete.

| Issue | What it identifies | Severity and scope |
|----|----|----|
| `incomplete_form_status` | An applicable form status is missing or is not Complete. | Info, `record_field` |

#### Consistency

Consistency checks compare stored values with their field definitions
and identify contradictory checkbox selections. These findings are tied
to either a specific record-field value or an aggregate checkbox
pattern.

| Issue | What it identifies | Severity and scope |
|----|----|----|
| `invalid_choice_value` | A raw radio, dropdown, Yes/No, or True/False code is absent from the field’s choices. | Warning, `record_field` |
| `invalid_validation_format` | A text value does not satisfy its configured REDCap validation. | Warning, `record_field` |
| `checkbox_no_values_selected` | No options are selected across assessed rows with explicit checkbox exports. | Info, `field` |
| `checkbox_none_with_other` | A none-like option is selected with another checkbox option. | Warning, `record_field` |

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
- `finding_count`: total findings from the selected check groups.

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
weighted missing rates by form. `summaries$fields` provides the detailed
missingness profile:

- `record_count` is the number of applicable rows assessed for the
  field. It is not necessarily the number of distinct records because
  events and repeating instruments can contribute multiple rows per
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
cells; they are not simple averages of field-level percentages.

#### Records and events

`summaries$records` contains one row per record. `missing_field_count`
sums missing applicable field values across that record’s events and
repeat instances. `finding_count` counts record-specific findings;
field-level findings do not belong to an individual record.

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

`metadata` preserves the standardized project structure used by the
checks.

| Element | Contents |
|----|----|
| `fields` | Standardized REDCap field metadata used by the report. |
| `forms` | One row per form with field, required-field, and choice-field counts. |
| `events` | Standardized longitudinal event metadata. |
| `event_instruments` | Standardized event-to-form mapping. |
| `instruments` | Instrument names and labels returned by REDCap. |
| `repeating_instruments` | Repeating event and instrument configuration returned by REDCap. |
| `choices` | Parsed explicit choices and implicit Yes/No and True/False choices. |
| `validation` | Fields with validation types or minimum/maximum bounds. |
| `branching` | Fields with branching logic and parsed referenced field names. |
| `record_id_field` | Field name used as the project record identifier. |
| `source` | Source of the normalized project; API reports use `"api"`. |

These tables support project-level review and help interpret findings:

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

## How Checks Work

Different check groups intentionally evaluate different sets of rows. In
this table, **form status** determines whether the row can enter the
check, **branching logic** determines whether a currently hidden field
is excluded, and **blank values** describes what happens after a row and
field are eligible.

| Evaluation | Form status | Branching logic | Blank values |
|----|----|----|----|
| Missingness | Only Unverified or Complete forms are evaluated. | A field is evaluated only where supported logic indicates it is shown. | A blank eligible value is counted as missing. |
| Choice and text validation | Stored values are evaluated for any form status, including Incomplete. | A stored value is evaluated even if branching currently hides the field. | Blank values do not produce choice or format findings. Eligible blanks are handled separately by missingness checks. |
| Checkbox consistency | Only Unverified or Complete forms are evaluated. | A checkbox is evaluated only where supported logic indicates it is shown. | Explicit `0` values are meaningful; rows where every option is blank are skipped. |
| Metadata | Record rows and form status are not used. | Branching logic does not control metadata checks. | A blank label or choice definition can trigger a metadata finding; blank record values are not examined. |

“Supported logic” refers to the subset of REDCap branching expressions
the package can evaluate, described under Core Assumptions.
Event-to-form mappings and repeating configuration are respected
whenever a check evaluates record data. For example, a blank field on a
Complete form counts as missing only when supported branching logic
shows that field. By contrast, a nonblank malformed value on an
Incomplete form can still produce a validation finding, even when
branching currently hides the field.

### Missingness

Missingness is evaluated in two stages. The report first determines
whether a field should be assessed on an exported row, then determines
whether the field has an observed value. This prevents structural blanks
from events, repeating instruments, branching logic, and unfinished
forms from being treated as missing data.

#### Form status

Raw REDCap form status determines whether a row contributes to
missingness:

| Form status | Included in missingness? | `incomplete_form_status`? |
|----|---:|---:|
| Incomplete (`0`) | No | Yes |
| Unverified (`1`) | Yes | Yes |
| Complete (`2`) | Yes | No |
| Blank value | No | Yes, when the form is otherwise applicable |
| Status column unavailable | No | No status finding can be generated |

If a form has no exported `<form_name>_complete` column, its fields have
no assessable rows and their missing rates are `NA`.

#### Field applicability

Among rows with an assessable status, applicability accounts for:

1.  Event-to-form mappings in longitudinal projects.
2.  Repeating event and instrument configuration.
3.  The repeating instrument associated with each exported row.
4.  Supported branching logic that determines whether the field is
    shown.

For applicable non-checkbox fields, `NA` and blank or whitespace-only
values are missing. A missing required field produces
`required_field_missing`. Any required or optional field can produce
`unexpected_sparse_field` when its missing rate meets
`sparse_threshold`.

#### Checkbox behavior

REDCap exports one `0`/`1` column per checkbox choice. The report
evaluates missingness at the parent-field level:

- One or more checked (`1`) choices are observed.
- All choices explicitly unchecked (`0`) are also observed. This is a
  meaningful no-selection state.
- The parent field is missing only when every exported choice value is
  `NA` or blank on an otherwise applicable row.

On an Incomplete form, checkbox exports are ignored for missingness. On
an Unverified or Complete form, an all-zero state is observed, while an
all-missing set can contribute to sparse-field and required-field
findings.

Checkbox consistency findings do not redefine the all-zero state as
missing:

- `checkbox_no_values_selected` is one field-level prompt when no option
  is selected across assessed rows with explicit checkbox exports.
- `checkbox_none_with_other` is a record-field warning when an option
  labeled “None”, “No”, “Not applicable”, “N/A”, “NA”, or “None of the
  above” is selected with another option.

### Metadata-defined validation

REDCap metadata records the validation selected by a project designer in
the REDCap field configuration UI. For text fields, the selected
validation type and optional minimum or maximum are exported in the
`text_validation_*` columns. Choice definitions configured in REDCap are
similarly exported as field metadata.

The report compares nonblank raw values with these user-configured
rules. It inspects structurally applicable rows regardless of whether a
form is Incomplete, Unverified, or Complete. Branching logic does not
suppress a stored value that violates its metadata rule.

#### Choice values

`invalid_choice_value` identifies raw codes absent from metadata choices
for radio, dropdown, Yes/No, and True/False fields. Yes/No and
True/False use their implicit REDCap `0`/`1` definitions.

Checkbox fields are excluded. Their metadata codes identify separate
option columns, while each exported cell contains a `0` or `1` selection
state.

#### Why the raw export is used

REDCap choice fields have a stored code and a displayed label.
Validation must compare the stored code with the codes defined in
metadata, so the report requests raw values rather than labels. For
example, a dropdown choice stored as code `1` remains `"1"` instead of
being replaced by its display label.

Automatic column type guessing is also disabled. This prevents a column
of `0` and `1` codes from being converted to logical `FALSE` and `TRUE`,
which would no longer match the REDCap choice definitions. Empty API
cells are imported as missing, while a literal code such as `"NA"` is
preserved as an actual value.

Fields without explicit radio, dropdown, or checkbox definitions produce
`missing_choice_definition` instead of an invalid-choice finding.

#### Text formats and bounds

`invalid_validation_format` applies only to `text` fields with a
supported `text_validation_type_or_show_slider_number`. The report does
not infer validation from R classes, field names, labels, or observed
values.

| Family | Metadata examples | Validation behavior |
|----|----|----|
| Integer | `integer` | Integer syntax; scientific notation is rejected. |
| Number | `number`, `number_1dp` through `number_4dp`, comma-decimal variants | Configured decimal mark and precision; a leading zero is optional. |
| Date | `date_dmy`, `date_mdy`, `date_ymd` | Canonical API value `YYYY-MM-DD`; impossible dates are rejected. |
| Datetime | DMY, MDY, and YMD `datetime_*` types | Canonical YMD export with minutes or seconds, according to the validation type. |
| Time | `time`, `time_hh_mm_ss`, `time_mm_ss` | Configured format and clock boundaries. |
| Email | `email` | Structural email format, not deliverability. |
| Phone | `phone`, `phone_australia` | Common punctuation and regional digit structure. |

The DMY, MDY, and YMD suffix controls REDCap’s data-entry display. API
exports still use canonical YMD date and datetime values. Number
validations accept values with or without a leading zero, such as
`0.850` and `.850`.

When provided, `text_validation_min` and `text_validation_max` produce
`outside_validation_range` for valid numeric, date, datetime, and time
values outside the configured bounds. Either bound can be used alone.
Malformed values produce `invalid_validation_format` but not range or
future-date findings.

Use issue names to review metadata-defined validation findings together.
The mock report contains a range violation but no malformed formats or
invalid choice codes.

``` r

validation_issues <- c(
  "invalid_choice_value",
  "invalid_validation_format",
  "outside_validation_range"
)

mock_quality_report$findings |>
  dplyr::filter(issue %in% validation_issues) |>
  dplyr::select(record_id, field_name, issue, value, expected)
#> # A tibble: 1 × 5
#>   record_id field_name issue                    value expected         
#>   <chr>     <chr>      <chr>                    <chr> <chr>            
#> 1 C002      age        outside_validation_range 999   Between 0 and 120
```

### Distribution and future-date checks

`numeric_iqr_outlier` is a distribution-based heuristic, not a REDCap
metadata rule. Its behavior depends on sample size, field distribution,
and `outlier_iqr_multiplier`.

`future_date` applies to valid date and datetime fields. It compares the
exported calendar date with
[`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) captured when the
report runs. A date after that system date is flagged; a datetime later
on the same calendar day is not. Malformed temporal values are handled
by `invalid_validation_format` instead.

## Core Assumptions

### Findings are review prompts

The checks identify items worth reviewing. They do not establish
clinical correctness, protocol compliance, or regulatory compliance. An
`info` finding can still be important, and a report with no findings
does not prove that the data are correct.

`finding_id` is assigned when a report is built and is not persistent
across runs. Use the issue and REDCap context columns when tracking
findings externally.

### Branching logic support is intentionally bounded

The branching evaluator supports ordinary field comparisons, checkbox
choice references, parentheses, and `and`/`or` combinations. A valid
expression that evaluates to `NA` is treated as not shown for that row.

REDCap functions, smart variables, and expressions outside the supported
subset are treated as applicable rather than used to hide a potentially
required value. The report does not produce findings for apparently
missing branching references because event-qualified fields, cross-form
fields, and smart variables cannot be classified reliably from bracketed
tokens alone.

### Counts reflect REDCap’s row structure

A REDCap record can produce multiple API rows because of events and
repeating instances. Project `record_count` counts distinct records,
`raw_row_count` counts exported rows, field `record_count` counts
applicable rows, and event `row_count` counts exported rows per event.
Choose the measure that matches the question being asked.

### Results reflect the current project export

Descriptive fields are removed before analysis because they do not store
record values. Structural metadata retrieval failures are reported as
errors rather than silently converted to empty project structure.
Results reflect the records and metadata available when the report is
built, including the token’s project access and export configuration.
