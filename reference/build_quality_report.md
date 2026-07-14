# Build a REDCap data quality report

`build_quality_report()` pulls a REDCap project through the API, applies
general data quality heuristics, and returns findings, summaries, and
interpreted metadata.

## Usage

``` r
build_quality_report(
  redcap_uri,
  token,
  checks = c("missingness", "metadata", "outliers", "operational", "consistency"),
  sparse_threshold = 0.95,
  outlier_iqr_multiplier = 3,
  progress = c("auto", "none", "show")
)
```

## Arguments

- redcap_uri:

  REDCap API URI.

- token:

  REDCap API token.

- checks:

  Character vector of check groups to run.

- sparse_threshold:

  Missingness threshold used to flag unexpectedly sparse fields.

- outlier_iqr_multiplier:

  Multiplier used for IQR-based numeric outlier detection.

- progress:

  Progress display mode. Use `"auto"` to show progress only in
  interactive sessions, `"none"` to suppress progress, or `"show"` to
  force progress output.

## Value

A list with `findings`, `summaries`, and `metadata` elements. `findings`
includes record, form, event, repeat instrument, and repeat instance
context when available from the REDCap export.
`summaries$project$raw_row_count` equals
[`nrow()`](https://rdrr.io/r/base/nrow.html) for the records returned by
the REDCap API. `summaries$project$field_count` is the count of distinct
standardized metadata field names. `summaries$fields$missing_rate` is
the field-level fraction missing among applicable rows.
`summaries$records` contains one row per record; its missing-field count
is summed across applicable event and repeat rows.
`summaries$forms$missing_rate` and `summaries$project$missing_rate` are
weighted fractions missing across applicable field-row cells.
Applicability accounts for form availability, event-form mapping,
repeating instrument structure, and branching logic. Missingness is only
assessed where the raw REDCap form status column `<form_name>_complete`
is `1`/Unverified or `2`/Complete. Status `0`/Incomplete is handled by
operational checks and does not contribute to missingness, even when
REDCap exports checkbox choices as `0`. Branching expressions that use
unsupported REDCap functions or smart variables are treated as
applicable so required values remain reviewable.

## Details

Current `findings$issue` values by `findings$check`:

- `missingness`

  - `required_field_missing`

  - `unexpected_sparse_field`

- `metadata`

  - `duplicate_field_label`

  - `missing_field_label`

  - `missing_choice_definition`

  - `orphaned_branching_reference`

  - `high_risk_free_text`

- `outliers`

  - `outside_validation_range`

  - `numeric_iqr_outlier`

  - `future_date`

- `operational`

  - `incomplete_form_status`

- `consistency`

  - `checkbox_none_with_other`

## Examples

``` r
if (FALSE) { # \dontrun{
report <- build_quality_report(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)
} # }
```
