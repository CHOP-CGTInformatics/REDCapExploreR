# Get Record Status Dashboard data

`get_record_status_data()` retrieves REDCap records and project
structure through the API, then returns a plotting-friendly tile table
similar to the high-level REDCap Record Status Dashboard.

## Usage

``` r
get_record_status_data(redcap_uri, token)
```

## Arguments

- redcap_uri:

  REDCap API URI.

- token:

  REDCap API token.

## Value

A tibble with one row per dashboard tile and these columns: the project
record ID field, `form_name`, and `pct_complete`. Longitudinal projects
also include `event_name`, the REDCap unique event name.

## Details

REDCap stores form status in one `*_complete` field per instrument. This
function normalizes those status values and joins them to a record/form
grid built from API records, metadata, instrument metadata, event
metadata, and event-instrument mapping.

For classic projects, the output contains one row per record and
instrument. For longitudinal projects, the output contains one row per
observed record/event/instrument combination enabled by the
event-instrument mapping. Observed record/event combinations are taken
from all API rows, including rows that only exist because a repeating
instrument was exported.

Repeating instrument instances are summarized into the same tile rather
than returned as separate rows. The tile's `pct_complete` value is the
proportion of relevant instances marked complete. Non-repeating
instruments are evaluated only on non-repeating rows to avoid structural
blanks from other repeating instruments.

Completion values are returned as proportions where `1` means all
relevant statuses are complete, `0` means no relevant statuses are
complete, and `NA` means no completion status was available for that
tile.

The record identifier column uses the REDCap project's record ID field
name, so a project with `infseq_id` as the record ID field returns an
`infseq_id` column. `form_name` is a factor ordered by REDCap event and
instrument order. In longitudinal projects, `form_name` uses
`"Event Label : Form Label"` when labels are available.

## Examples

``` r
if (FALSE) { # \dontrun{
get_record_status_data(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)
} # }
```
