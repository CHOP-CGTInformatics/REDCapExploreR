# Field relationship summary implementation plan

Implement `field_relationship_summary(field, codebook, include_diagnostics = FALSE)` as a read-only report of
one field's definition, upstream inputs, direct references, downstream connections,
and explanations of how those relationships work. It uses current project
metadata without fetching records or comparing historical codebooks.

The acceptance example is `weight_kg`: identify its calculation, branching,
annotation, piping, and embedding references; explain indirect relationships;
and retain the exact metadata evidence behind every result. Present the report
as an explanation of project relationships, with no deletion scenarios, risk
scores, or predictions of changes to stored values.

## Public workflow and metadata

```r
codebook <- build_codebook(redcap_uri, token)
relationships <- field_relationship_summary(field = "weight_kg", codebook = codebook)
relationships
relationships$references
view_field_relationship_summary(relationships)
save_field_relationship_summary(relationships, "weight-relationships.html")
```

A codebook supplies the field definitions, expressions, and event/repeat context
needed to interpret a field name. Building it once lets users explore several
fields from the same project metadata without repeated API calls. Record data
is not needed. Include this short rationale in the function's `@details` and the
getting-started example before the first call.

- Export `field_relationship_summary(field, codebook, include_diagnostics = FALSE)` with an S3 result class of
  `redcap_field_relationship_summary`. Retain the user-selected function name;
  internal data-processing functions use `get_`.
- Require one nonmissing, nonblank field name that exactly matches a field in
  the codebook. Accept dictionary field names, including checkbox parent fields;
  reject labels, checkbox export column names, and unknown names with actionable
  errors. Do not silently substitute approximate matches.
- Validate the codebook and definition schema once at the public boundary,
  including unique nonblank dictionary field names. Downstream processing assumes
  valid typed inputs. Missing optional metadata is a coverage condition.
- Add a `definitions` element to codebooks containing `fields`, `project`,
  `source_columns`, and `schema_version = 1L`. `fields` retains the complete
  standardized dictionary, including descriptive fields, calculations,
  annotations, section headers, notes, choices, and optional source columns.
  Preserve expression strings, punctuation, HTML, and whitespace exactly.
  `project` retains the full standardized project-information table.
- Record which source columns were actually returned before normalization adds
  defaults. This distinguishes unavailable annotation or section-header metadata
  from available columns whose cells are blank.
- Reuse the existing event, instrument, event-assignment, and repeating metadata.
  Retain the canonical event key in codebook repeating configuration so analysis
  does not depend on whether a particular API alias was returned.
- Keep existing codebook field tables, public signatures, and viewing/saving
  behavior compatible. Older codebooks remain viewable; the relationship function
  rejects codebooks lacking the definition schema and instructs users to rebuild.
- Relationship analysis uses only the supplied object and makes no API calls.
  Capturing definitions uses the existing metadata retrieval endpoints and adds
  no record export, credentials, or snapshot-comparison workflow.

## Relationship extraction and traversal

Build an internal occurrence table for the project before selecting the field's
relationships. An occurrence records a reference to a source field within a
dependent item. The edge direction is always source field to dependent item.

### Metadata locations and interpretation

| Location | First-release treatment |
| --- | --- |
| `branching_logic` | Extract field and checkbox references outside ordinary quoted literals; classify as visibility relationships. |
| Calculation expressions on `calc` fields | Extract references without evaluating formulas; classify as calculation-input relationships. Do not parse choice definitions as formulas. |
| `field_annotation` | Recognize `@DEFAULT`, `@CALCTEXT`, and `@CALCDATE`; extract their field references with the argument's expression or piping context. Support nested `@IF` conditions and recognized tag branches. Preserve complete annotations even when no field references occur. |
| Field labels, field notes, section headers, choice labels | Extract supported piping references and basic `{field_name}` embedding. Preserve the owning metadata location and choice code where relevant. |
| Project custom record label | Extract supported piping; represent the dependent as a project setting rather than a synthetic field. |
| Other project settings and unknown annotation tags | Retain source text and report unsupported coverage. Candidate references are explicitly unclassified and do not become confirmed relationships. |

Use location-specific scanners with token spans rather than one global bracket
regular expression. Reuse quote/token handling from `R/branching.R` where it
matches reference syntax, keeping relationship extraction separate from branching
evaluation. A complex formula can have resolved references even when its result
cannot be evaluated by the package.

- Recognize ordinary `[field]`, checkbox `[field(code)]`, explicit
  `[event][field]`, explicit numeric instance qualifiers, and supported piping
  modifiers. Preserve the original reference text and all qualifiers.
- Resolve field identity separately from event and instance context. Resolve
  explicit events against unique event names and instruments against their
  mapping. Unqualified references retain a relative current-context qualifier;
  do not invent a single event for a field used in several events.
- Mark smart-variable event/instance selectors as context-dependent when field
  identity can still be identified. Support combined checkbox modifiers `:checked:value` and `:unchecked:value`.
  Keep known field references traversable when qualifiers need interpretation;
  unknown fields and ambiguous annotation candidates do not create edges.
- Resolve checkbox codes against the parent's choices. Missing fields, missing
  choice codes, and invalid explicit event references are unresolved references,
  with the original expression retained for review.
- In calculation and branching expressions, quoted text resembling a reference
  is a literal. In piping-enabled arguments such as a quoted `@DEFAULT` value,
  references are extracted. Ambiguous quoted constructs inside action-tag
  calculations remain unclassified until a documented fixture establishes their
  interpretation; report the coverage limitation rather than inventing behavior.
- Keep one row per reference occurrence, including repeated uses in a single
  expression. Assign deterministic IDs from dictionary order, location order,
  and token order within the supplied object. Document that IDs are report-local.
- Traverse upstream and downstream independently. Upstream fields are context,
  and downstream items describe where the selected field is used. Other inputs
  to a downstream calculation remain visible in its expression but do not enter
  the related-field set merely because they share that calculation.
- Include every reachable related edge once and count related items distinctly.
  Record minimum depth and one deterministic shortest evidence path per item.
  Preserve all alternative edges in the occurrence table instead of enumerating
  exponentially many paths. Handle disconnected fields, cycles, and self-links
  without recursion loops; describe reciprocal connections without automatically
  classifying them as invalid project design.
- Keep structural context separate from field-reference edges. Sharing an
  instrument, event, or matrix does not establish a reference relationship.

## Report and viewer

Return a list with seven stable components. Empty results use typed empty tibbles.

| Component | Contents |
| --- | --- |
| `field` | One row with the full selected definition, display label, instrument, event assignments, and repeating context. Include all annotations, even tags with no references. |
| `upstream` | One row per reachable upstream field, its definition/context, minimum depth, and a representative evidence path. |
| `references` | One row per related reference occurrence across the upstream and downstream subgraphs, including direct references. Provide source field, dependent kind/name, relationship, metadata column, owning choice code, full expression, token text/span, event/instance qualifiers, resolution status/reason, and traversal role. |
| `downstream` | One row per reachable dependent field or project setting, minimum depth, direct/indirect classification, its definition/context where applicable, and a representative evidence path. |
| `summary` | Readable explanations grouped by related item, metadata location, exact expression, relationship type, and interpretation, with direction, direct/indirect classification, supporting occurrence IDs, and a connecting path. No deployment predictions or risk ratings. |
| `issues` | Related interpretation issues with item, metadata location, exact expression, and reason, always available without diagnostics. |
| `coverage` | Project-wide source counts, aggregate limits elsewhere, boundaries, and definitions/parser versions. Full project-wide `diagnostics` is an optional eighth component when `include_diagnostics = TRUE`. |

A field with no outgoing relationships still returns its definition, upstream
context, and coverage. Unresolved candidate references whose field identity is
known remain visible in the related evidence, without asserting resolved context.
Project-wide interpretation limits are disclosed in aggregate in coverage because
they can conceal connections. Original related issue evidence is always retained. Full project-wide source and reference rows are retained only
when `include_diagnostics = TRUE`.

### Relationship language

Explain how the selected field connects to each item. Use categories such as
Calculation, Branching logic, Piping, Default value,
Field embedding, and Action-tag condition, retaining specific action tags. Keep relationship type separate from
interpretation status. Use Upstream and Downstream in both the R output and
viewer, with short descriptions explaining their direction.

- Use specific statements: "bmi uses weight_kg as a calculation input,"
  "weight_review references weight_kg in its branching logic," and
  "starting_weight references weight_kg when assigning its default value."
- For indirect connections, explain the path: "review_notes is connected through
  bmi and review_required." Identify each relationship along that path.
- Preserve differences in behavior: a default is initialization rather than
  continuous synchronization, embedding concerns presentation, and a calculation
  reference does not establish when stored results were last recalculated.
- A visibility relationship does not prove that an existing stored value changes.
  Describe links supported by expressions without inferring clinical meaning,
  units, or outcomes from field names.
- Use "unresolved reference" or "context-dependent relationship" for uncertainty.
  Do not use change/removal warnings as the report's organizing language.
- For an empty result say "No references to weight_kg were found in the inspected
  metadata." Show source coverage beside that result. Alerts, survey queues,
  report filters, external modules/scripts, and inaccessible configuration are
  not inspected in this release.

### Presentation

- Add `print.redcap_field_relationship_summary()` showing the field/instrument,
  distinct direct and indirect related-item counts, relationship counts, and
  coverage issues.
  Avoid counting multiple occurrences as multiple related fields.
- Export `view_field_relationship_summary(relationships, page_length = 25)` and
  `save_field_relationship_summary(relationships, file, page_length = 25)`,
  matching existing HTML viewing and single-file saving conventions.
- Present Field, Upstream, Downstream, Summary, References, and Coverage sections.
  Include Interpretation issues when relevant issues exist. Keep item inventories
  concise; retain full definitions, parser offsets, IDs, and qualifier columns in R. Show all references
  initially, with optional column filters. Use readable relationship-type labels
  and notes for non-resolved interpretations while retaining machine-readable
  codes in R output. Add a Diagnostics section only when the report was
  constructed with `include_diagnostics = TRUE`. Provide full expressions and
  evidence-path details on demand.
- Extract only genuinely shared rendering/table and HTML-saving behavior from
  the codebook viewer. Scope JavaScript selectors and section identifiers to each
  viewer root so multiple viewers on the same page operate independently.
- Keep CSS/JavaScript under `inst/`, escape metadata as text, and retain keyboard
  operation and accessible section navigation. Saved HTML works offline with
  inlined local dependencies. A graph visualization is deferred; tables and
  evidence paths are the complete first-release interaction.
- Use existing DT/htmltools dependencies. Keep extraction, traversal, interpretation,
  and UI in separate responsibility-based modules; add no graph dependency.

## Implementation sequence and acceptance tests

1. Extend codebook definitions and availability tracking, update roxygen output
   documentation, and regenerate the bundled codebook through `data-raw/mock-data.R`.
   Confirm all existing public workflows remain compatible.
2. Implement source-specific extraction and a typed occurrence table. Verify
   direct-reference evidence before adding graph traversal.
3. Add cycle-safe traversal, the seven-component report, relationship explanations,
   and S3 printing. Assert results against independently specified synthetic fixtures.
4. Add the HTML viewer and saving workflow; refactor shared presentation only
   where it removes duplication without coupling the report calculations to UI.
5. Document the public workflow, support boundaries, and examples; update NEWS,
   package reference navigation, and any intentionally changed asset snapshots.

### Focused unit tests

Organize tests by responsibility. Keep each `test_that()` focused on one behavior
and use short names that state the expected result. Build expected results
independently of the extraction and traversal implementation. The test filenames
below are under `tests/testthat/`.

| Test location | Units and essential cases |
| --- | --- |
| Existing `test-codebook.R` | Complete definitions retain expression/annotation text and optional columns; source availability differs from blank values; old codebooks remain viewable. |
| `test-field-relationship-references.R` | Exact field/checkbox references; quoted literals versus piping-enabled strings; supported annotations and nested conditions; piping/embedding source locations; explicit and dynamic event/instance qualifiers; unresolved or unsupported syntax. |
| `test-field-relationship-summary.R` | Input/schema errors; upstream versus downstream direction; direct and indirect links; duplicate occurrences versus distinct items; alternative paths, self-links, cycles, disconnected fields; stable empty schemas and explanatory print output. |
| `test-field-relationship-viewer.R` | Section/filter behavior, coverage and empty messages, escaped expressions, independent viewer roots, and standalone HTML saving. |

Test-authoring expectations:

- Use the smallest tibble that expresses each unit case. Keep fixture values
  close to the assertions; introduce a local fixture helper only when several
  tests share meaningful setup. Avoid a general fixture framework or wrappers
  around the public functions under test.
- Use table-driven cases only for variations of the same behavior, such as
  reference syntax. Do not combine unrelated extraction, traversal, rendering,
  and API behavior in one test.
- Assert relevant rows, columns, relationships, and exact expressions directly.
  Avoid broad whole-object snapshots that obscure the behavior being checked.
  Reserve asset snapshots for intentional viewer CSS/JavaScript changes.
- Add one compact weight/height/BMI integration example connecting calculations,
  branching, annotations, and display text. Assert its expected related items,
  occurrence counts, and representative paths; height must not appear related
  merely because it shares BMI with weight.
- Check annotations with no references remain in the field definition. Verify
  quoted `@DEFAULT` piping is discovered while bracket-like text in an ordinary
  formula literal is not. Ambiguous annotation expressions disclose uncertainty.
- Verify project-setting consumers remain distinct from fields, and real field
  identity remains distinguishable from invalid or dynamic event context.
- Assert neutral explanations for calculations, visibility, defaults, piping,
  and embedding. Do not introduce tests for deletion predictions or risk scores.
- Check that report construction makes no API calls and metadata is never
  executed. Exercise viewer filtering and keyboard navigation in an allowed
  browser environment; asset snapshots alone are not behavioral verification.

Run R work with `Rscript -e` and initialize package sessions with
`devtools::load_all()`. Run targeted and full testthat suites, `devtools::document()`,
lintr, non-rewriting styler verification, `R CMD check --no-manual` including
vignettes, and a pkgdown build. Use only dependencies listed in DESCRIPTION in
tests. Verify saved HTML offline and document any remaining browser-validation
limitation. The feature is complete when the weight example explains its known
relationships, issues, and coverage, and existing package tests pass without unexplained
output or snapshot changes.

## Source layout

- `R/field_relationship_summary.R`: report construction and validation, graph
  traversal, relationship explanations, shared interpretation labels, and printing.
- `R/relationship_references.R`: metadata source assembly, token and annotation
  extraction, field/qualifier resolution, and project reference occurrences.
- `R/relationship_coverage.R`: source coverage, related interpretation issues,
  and optional project-wide diagnostics.
- `R/relationship_viewer.R`: HTML presentation, display labels, and saving.
