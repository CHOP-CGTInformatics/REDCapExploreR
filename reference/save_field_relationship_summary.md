# Save a field relationship viewer

Writes a single HTML file with local dependencies inlined for offline
exploration and sharing.

## Usage

``` r
save_field_relationship_summary(relationships, file, page_length = 25)
```

## Arguments

- relationships:

  A report from
  [`field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/field_relationship_summary.md).

- file:

  Output HTML file path; its directory must exist.

- page_length:

  Number of rows shown per table page.

## Value

The output path, invisibly.
