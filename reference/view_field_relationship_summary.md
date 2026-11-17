# View a field relationship summary

Displays definitions, references, connections, and coverage as
searchable HTML tables with original expressions and evidence paths.

## Usage

``` r
view_field_relationship_summary(relationships, page_length = 25)
```

## Arguments

- relationships:

  A report from
  [`field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/field_relationship_summary.md).

- page_length:

  Number of rows shown per table page.

## Value

An `htmltools` browsable HTML object.

## Examples

``` r
relationships <- field_relationship_summary("weight_kg", mock_codebook)
if (FALSE) { # \dontrun{
view_field_relationship_summary(relationships)
} # }
```
