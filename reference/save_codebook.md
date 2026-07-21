# Save a REDCap codebook viewer

`save_codebook()` saves the interactive HTML viewer from
[`view_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/view_codebook.md)
to disk as a single HTML file. Local CSS and JavaScript dependencies are
inlined so the file can be opened in a browser or attached to an email
without a separate dependency folder.

## Usage

``` r
save_codebook(codebook, file, page_length = 25)
```

## Arguments

- codebook:

  A `redcap_codebook` object returned by
  [`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md).

- file:

  Output HTML file path.

- page_length:

  Number of rows to show per interactive table page.

## Value

The output file path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
codebook <- build_codebook(
  redcap_uri = Sys.getenv("REDCAP_URI"),
  token = Sys.getenv("REDCAP_TOKEN")
)

save_codebook(codebook, "codebook.html")
} # }
```
