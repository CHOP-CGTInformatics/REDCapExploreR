# Summarize how a REDCap field is related to project metadata

Identifies upstream references and downstream uses of one REDCap field
using a codebook's metadata. Helps reviewers explore project structure
and locate configuration to inspect before changing a field.

## Usage

``` r
field_relationship_summary(
  field,
  codebook,
  include_diagnostics = FALSE,
  include_inspection_log = FALSE
)
```

## Arguments

- field:

  One exact dictionary field name, including checkbox parent names.

- codebook:

  A codebook returned by
  [`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md).

- include_diagnostics:

  Include project-wide interpretation issues as an additional
  `diagnostics` tibble (Project Diagnostics). Default is `FALSE`.

- include_inspection_log:

  Include every inspected metadata location and reference diagnostic as
  an additional `inspection_log` tibble. Default is `FALSE`. This is
  independent of `include_diagnostics`.

## Value

A list with classes `redcap_field_relationship_summary` and `list`,
containing seven tibbles by default:

- `field`: the selected field's definition and context.

- `upstream`: fields it references, directly or indirectly.

- `downstream`: fields and project settings that reference it, directly
  or indirectly.

- `references`: related reference occurrences with original expressions.

- `summary`: explanations grouped by metadata location, expression, and
  reference interpretation.

- `issues`: interpretation issues in selected or related metadata.

- `coverage`: project-wide source counts, interpretation limits, and
  exclusions.

Optional tibbles are `diagnostics` (project-wide interpretation issues,
when `include_diagnostics = TRUE`) and `inspection_log` (the full
inspection inventory, when `include_inspection_log = TRUE`).

## Details

A codebook supplies field definitions, expressions, and event/repeat
context needed to interpret references. Build it once to explore several
fields without repeated API calls. Record data is not needed. Older
codebooks without complete definitions must be rebuilt.

Use dictionary field names rather than labels or exported checkbox
columns. For example, supply `symptoms`, not `symptoms___fever`.

## Reading relationships

Upstream means what the selected field references; downstream means
where it is referenced. Each related item appears once, with its minimum
depth and one representative shortest reference chain. Alternative
related references remain in `references`.

`references` has one row per occurrence; `summary` groups occurrences,
so their row counts can differ. Chains describe field-level connections,
not continuous paths through particular events or repeating instances.

## Interpreting references

`relationship` identifies the connection type, while `field_matched`
records whether the referenced field exists in the dictionary.
`resolution` describes syntax and qualifier interpretation, not data
quality.

`status_note` appears as **Interpretation note** in the viewer. It
explains unresolved, context-dependent, or unclassified references. A
blank note means no interpretation limit was detected; it does not
validate the configuration. Notes apply to individual occurrences in
`references`, grouped evidence in `summary`, and the displayed chain in
`upstream` and `downstream`. In `issues` and `diagnostics`, they explain
issues at metadata locations.

Known field references remain linked when qualifiers need
interpretation. Unknown fields and unsupported annotation candidates do
not create traversable links; their evidence remains available in
interpretation issues.

## Checkbox references

The report includes whole-checkbox and individual-option references.
`reference_choice` and `reference_choice_label` identify the referenced
option. `choice_code` identifies the containing choice label when an
expression occurs in a choice label; it is not the referenced option.

## Inspection scope

Inspected sources include branching logic, calculations, supported field
annotations, piping, embedding, and the custom record label. The report
does not evaluate expressions, predict changes to stored values, or
establish referential integrity. Configuration outside the inspected
metadata, such as alerts and external scripts, may contain additional
uses.

`coverage` and optional `diagnostics` describe the entire project and
are identical for every field using the same codebook. `issues` retains
details relevant to the selected field and related items. The optional
flags control returned detail, not analysis or API calls.

## See also

[`build_codebook()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/build_codebook.md),
[`view_field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/view_field_relationship_summary.md),
[`save_field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/save_field_relationship_summary.md)

## Examples

``` r
relationships <- field_relationship_summary("weight_kg", mock_codebook)
relationships
#> <REDCap field relationships>
#> Field: weight_kg (follow_up)
#> Upstream: 1 item
#> Downstream: 6 direct items, 1 indirect item
#> Reference occurrences across returned chains: 12
#> Action-tag condition: 1; Calculation: 1; Piping: 4; Field embedding: 1; Default value: 3; Unknown relationship type: 1; Branching logic: 1
#> Interpretation issues: 3 related metadata locations (see issues).
#> Project Coverage: project-wide metadata inspection; see coverage for sources and exclusions.
#> Indirect connections are reference chains, not predictions of value or behavior changes.
#> Project interpretation limits are summarized in coverage.
relationships$summary
#> # A tibble: 12 × 26
#>    source_field       dependent_kind  dependent_name   source_id metadata_column
#>    <chr>              <chr>           <chr>                <int> <chr>          
#>  1 baseline_weight_kg field           weight_kg               70 field_annotati…
#>  2 weight_kg          field           bmi                     80 select_choices…
#>  3 bmi                field           weight_review           85 branching_logic
#>  4 weight_kg          field           visit_summary           92 field_label    
#>  5 weight_kg          field           visit_summary           92 field_label    
#>  6 weight_kg          field           visit_summary           93 field_note     
#>  7 weight_kg          field           starting_weight         96 field_annotati…
#>  8 weight_kg          field           starting_weight         96 field_annotati…
#>  9 weight_kg          field           weight_at_entry        101 field_annotati…
#> 10 weight_kg          field           diagnostic_exam…       106 field_annotati…
#> 11 weight_kg          field           diagnostic_exam…       107 field_label    
#> 12 weight_kg          project_setting custom_record_l…       124 custom_record_…
#> # ℹ 21 more variables: choice_code <chr>, expression <chr>, relationship <chr>,
#> #   action_tag <chr>, field_matched <lgl>, resolution <chr>,
#> #   traversal_role <chr>, event <chr>, instance <chr>, modifier <chr>,
#> #   reference_choice <chr>, reference_choice_label <chr>,
#> #   occurrence_ids <list>, occurrence_count <int>, reason <chr>, direct <lgl>,
#> #   explanation <chr>, status_note <chr>, path <list>, field_path <list>,
#> #   reference_chain <chr>
```
