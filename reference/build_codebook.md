# Build a REDCap codebook

`build_codebook()` pulls REDCap project metadata through the API and
returns a structured, tidy codebook intended to resemble the information
users see in the REDCap UI codebook while remaining useful for
downstream R workflows.

## Usage

``` r
build_codebook(redcap_uri, token)
```

## Arguments

- redcap_uri:

  REDCap API URI.

- token:

  REDCap API token.

## Value

A list with classes `redcap_codebook` and `list`, containing:

- `fields`: one row per field, with display-oriented metadata and
  configuration summaries.

- `choices`: one row per coded choice option.

- `forms`: one row per instrument/form.

- `events`: one row per REDCap event when the project is longitudinal.

- `event_instruments`: one row per event/form mapping when available.

- `repeating`: repeating instrument/event configuration when available.

- `project`: project-level summary metadata.

- `definitions`: a list of complete standardized field/project metadata,
  source column availability, and a schema version.

The elements other than `definitions` are tibbles.

## Details

Only project metadata is retrieved; records are not exported. Counts
describe configured fields, instruments, and events, not populated data.
Reuse the returned codebook with
[`view_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/view_codebook.md)
or
[`field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/field_relationship_summary.md)
without additional API calls.

Branching logic, calculations, and field annotations are retained as
configuration text. The codebook does not evaluate expressions or
validate their behavior.

## Metadata representation

`fields` provides selected, display-oriented summaries.
`definitions$fields` and `definitions$project` retain complete
standardized metadata, including additional returned columns, for
downstream analysis. Column names and selected flags are normalized;
these are not unmodified copies of the API responses.

`definitions$source_columns` records which standardized columns were
actually returned, before missing columns are filled during
normalization. This distinguishes an unavailable metadata source from an
available but blank one.

`field_order` follows metadata row order, `form_order` follows each
instrument's first appearance, and `choice_order` follows the listed
option order.

## Choices and checkbox exports

`choices` contains one row per coded option for checkbox, radio,
dropdown, Yes/No, and True/False fields. Yes/No and True/False options
are supplied implicitly. Calculations are not parsed as choices. Choice
codes are retained as character values, including leading zeros.

For checkbox rows in `choices` (`choice_type == "checkbox"`), combine
`field_name` and the raw `choice_value` with three underscores:
`paste0(field_name, "___", choice_value)`. This gives the expected raw
export column name, such as `symptoms___fever`. Use the choice code, not
its label. `field_name` remains the parent dictionary field name.

## Events and repeating configuration

In `fields`, event counts and names summarize the events mapped to the
field's instrument. Repeating status summarizes instrument-level and
event-level repeating configuration across events; it may differ by
event. Use `event_instruments` and `repeating` for the detailed context
rather than treating a field's summary as applying uniformly to every
event.

Classic projects have empty event and event-instrument tables.
Documented API responses indicating non-applicable configuration produce
empty tables; permission and other retrieval failures are reported as
errors.

## Examples

``` r
if (FALSE) { # \dontrun{
codebook <- build_codebook(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)

codebook$fields
codebook$choices
} # }
```
