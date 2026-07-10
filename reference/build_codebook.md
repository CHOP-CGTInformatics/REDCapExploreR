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

A `redcap_codebook` list with these elements:

- `fields`: one row per REDCap field with display-oriented metadata,
  parsed choice summaries, validation, branching logic, event
  applicability, and repeating status when available.

- `choices`: one row per parsed choice option.

- `forms`: one row per instrument/form.

- `events`: one row per REDCap event when the project is longitudinal.

- `event_instruments`: one row per event/form mapping when available.

- `repeating`: repeating instrument/event configuration when available.

- `project`: project-level summary metadata.

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
