# View a REDCap codebook as interactive HTML tables

`view_codebook()` turns a `redcap_codebook` object from
[`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md)
into a static HTML viewer with tab-style section navigation and one
interactive table per codebook section. The returned object prints in
the RStudio/Posit Viewer. Use
[`save_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/save_codebook.md)
to save an HTML file for sharing or email attachment. Event,
event-instrument, and repeating-structure sections are omitted when
those API tables are empty.

## Usage

``` r
view_codebook(codebook, page_length = 25)
```

## Arguments

- codebook:

  A `redcap_codebook` object returned by
  [`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md).

- page_length:

  Number of rows to show per interactive table page.

## Value

An `htmltools` browsable HTML object containing interactive `DT` tables.

## Examples

``` r
if (FALSE) { # \dontrun{
codebook <- build_codebook(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)

viewer <- view_codebook(codebook)
save_codebook(codebook, "codebook.html")
} # }
```
