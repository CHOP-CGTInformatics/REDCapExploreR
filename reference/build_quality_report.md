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

  Number greater than 0 and less than or equal to 1. A field is flagged
  as unexpectedly sparse when its missing rate among applicable rows is
  greater than or equal to this value. The default `0.95` flags fields
  with at least 95 percent missing values.

- outlier_iqr_multiplier:

  Positive number used to define IQR outlier bounds as Q1 minus this
  value times the IQR and Q3 plus this value times the IQR. Smaller
  values produce narrower bounds and more findings. The default is `3`.

- progress:

  Progress display mode. `"auto"` enables normally throttled progress
  updates only in interactive sessions. `"none"` suppresses progress.
  `"show"` enables progress in all sessions and forces every update to
  render.

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
operational checks and does not contribute to missingness. Checkbox
parent fields are observed when their choice exports contain explicit
values, including an all-zero no-selection state; only an all-missing
set of choice exports is missing. Branching expressions that use
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

  - `high_risk_free_text`

- `outliers`

  - `outside_validation_range`

  - `numeric_iqr_outlier`

  - `future_date`

- `operational`

  - `incomplete_form_status`

- `consistency`

  - `invalid_choice_value`

  - `invalid_validation_format`

  - `checkbox_no_values_selected`

  - `checkbox_none_with_other`

`invalid_validation_format` is evaluated only for text fields with a
supported validation configured in
`text_validation_type_or_show_slider_number`. Supported validations
include REDCap integer and number formats, dates and datetimes, times,
email addresses, and North American and Australian phone numbers. Number
formats accept decimal values with or without a leading zero. Date and
datetime values are validated in REDCap's canonical API export formats,
regardless of their configured data-entry display order.
`outside_validation_range` enforces the optional `text_validation_min`
and `text_validation_max` bounds using the configured validation type.
Unvalidated text fields and unsupported validation types are not
assessed. `invalid_choice_value` applies to radio, dropdown, Yes/No, and
True/False fields; checkbox fields retain their dedicated consistency
checks. Choice and text validation checks use structurally applicable
form, event, and repeat rows regardless of form completion status or
branching logic.

## Examples

``` r
if (FALSE) { # \dontrun{
report <- build_quality_report(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)
} # }
```
