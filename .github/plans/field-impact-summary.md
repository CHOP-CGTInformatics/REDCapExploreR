# Field impact summary implementation plan

Implement `field_impact_summary(field, codebook)` as a read-only report of one
field's definition, upstream dependencies, direct references, downstream
relationships, and change or removal concerns. It uses current project metadata
without fetching records or comparing historical codebooks.

The acceptance example is `weight_kg`: identify its calculation, branching,
annotation, piping, and embedding references; explain indirect relationships;
and retain the exact metadata evidence behind every result.

## Public workflow and metadata

```r
codebook <- build_codebook(redcap_uri, token)
impact <- field_impact_summary(field = "weight_kg", codebook = codebook)
impact
impact$references
view_field_impact_summary(impact)
save_field_impact_summary(impact, "weight-impact.html")
```

- Export `field_impact_summary(field, codebook)` with an S3 result class of
  `redcap_field_impact_summary`. Retain the user-selected function name; do not
  add a forwarding `build_` alias. Internal data-processing functions use `get_`.
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
  behavior compatible. Older codebooks remain viewable; the new impact function
  rejects codebooks lacking the definition schema and instructs users to rebuild.
- Dependency analysis uses only the supplied object and makes no API calls.
  Capturing definitions uses the existing metadata retrieval endpoints and adds
  no record export, credentials, or snapshot-comparison workflow.

## Dependency extraction and traversal

Build an internal occurrence table for the project before selecting the field's
relationships. An occurrence records a reference to a source field within a
dependent item. The edge direction is always source field to dependent item.

### Metadata locations and interpretation

| Location | First-release treatment |
| --- | --- |
| `branching_logic` | Extract field and checkbox references outside ordinary quoted literals; classify as visibility dependencies. |
| Calculation expressions on `calc` fields | Extract references without evaluating formulas; classify as calculated-value dependencies. Do not parse choice definitions as formulas. |
| `field_annotation` | Recognize `@DEFAULT`, `@CALCTEXT`, and `@CALCDATE`; extract their field references with the argument's expression or piping context. Support nested `@IF` conditions and recognized tag branches. Preserve complete annotations even when no field references occur. |
| Field labels, field notes, section headers, choice labels | Extract supported piping references and basic `{field_name}` embedding. Preserve the owning metadata location and choice code where relevant. |
| Project custom record label | Extract supported piping; represent the dependent as a project setting rather than a synthetic field. |
| Other project settings and unknown annotation tags | Retain source text and report unsupported coverage. Candidate references are explicitly unclassified and do not become confirmed dependencies. |

Use location-specific scanners with token spans rather than one global bracket
regular expression. Reuse quote/token handling from `R/branching.R` where it
matches reference syntax, keeping dependency extraction separate from branching
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
  identity can still be identified. Unrecognized modifier forms or ambiguous
  syntax become unresolved candidates rather than guessed edges.
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
  not fields asserted to be affected. Other inputs to a downstream calculation
  are visible in its expression but do not enter the affected set merely because
  they share that calculation.
- Include every reachable related edge once and count affected items distinctly.
  Record minimum depth and one deterministic shortest evidence path per item.
  Preserve all alternative edges in the occurrence table instead of enumerating
  exponentially many paths. Handle disconnected fields, cycles, and self-links
  without recursion loops; identify cycles as review concerns, not automatically
  invalid project design.
- Keep structural context separate from field-reference edges. Sharing an
  instrument, event, or matrix does not make two fields dependencies.

## Report and viewer

Return a list with six stable components. Empty results use typed empty tibbles.

| Component | Contents |
| --- | --- |
| `field` | One row with the full selected definition, display label, instrument, event assignments, and repeating context. Include all annotations, even tags with no references. |
| `upstream` | One row per reachable upstream field, its definition/context, minimum depth, and a representative evidence path. |
| `references` | One row per related reference occurrence across the upstream and downstream subgraphs, including direct references. Provide source field, dependent kind/name, relationship, metadata column, owning choice code, full expression, token text/span, event/instance qualifiers, resolution status/reason, and traversal role. |
| `downstream` | One row per reachable dependent field or project setting, minimum depth, direct/indirect classification, its definition/context where applicable, and a representative evidence path. |
| `review` | Scenario-specific review concerns, affected item, explanation, evidence occurrence IDs, connecting path, and interpretation status. |
| `coverage` | Inspected source/location counts, extraction support, unavailable columns, unresolved or unclassified constructs with their locations, and definitions/parser versions. |

A field with no outgoing relationships still returns its definition, upstream
context, and coverage. Unresolved candidate references whose field identity is
known remain visible in the related evidence, without asserting resolved context.
Project-wide unresolved constructs remain discoverable through coverage because
they can limit a claim that no references were found.

### Impact wording

Generate review prompts for removal/rename, value changes, and definition changes.
The function receives no proposed replacement definition, so these are dependency
review scenarios, not simulated outcomes or detected amendments.

- Removal/rename: review explicit references and their downstream paths.
- Value change: identify calculated-value, visibility, and display consumers.
  Describe default/copy relationships as initialization behavior rather than
  continuous synchronization.
- Definition change: identify consumers whose expressions assume numeric type,
  coding, units, validation, event availability, or presentation. Only state an
  assumption when supported by the expression or definition; do not infer units
  or clinical meaning from a field name alone.
- Preserve relationship semantics along indirect paths. A visibility relationship
  does not prove a stored value changes, embedding concerns presentation, and a
  calculation reference does not prove stored results have been recalculated.
- Use categories such as reference review, calculation review, visibility review,
  presentation review, and unresolved scope. Do not calculate a global risk score.
- Distinguish confirmed references from potential effects. Use wording such as
  "Uses weight_kg as a calculation input" and "Review this expression before
  removing or renaming weight_kg."
- For an empty result say "No references to weight_kg were found in the inspected
  metadata." Show source coverage beside that result. Alerts, survey queues,
  report filters, external modules/scripts, and inaccessible configuration are
  not inspected in this release.

### Presentation

- Add `print.redcap_field_impact_summary()` showing the field/instrument, distinct
  direct and indirect dependent counts, relationship counts, and coverage issues.
  Avoid counting multiple occurrences as multiple affected fields.
- Export `view_field_impact_summary(impact, page_length = 25)` and
  `save_field_impact_summary(impact, file, page_length = 25)`, matching existing
  HTML viewing and single-file saving conventions.
- Present Definition, References, Upstream, Downstream, Review, and Coverage
  sections. Default the reference table to direct outgoing references; offer
  controls for relationship, traversal role, and resolution status, plus search
  and sorting. Provide full expressions and evidence-path details on demand.
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
3. Add cycle-safe traversal, the six-component report, review interpretation, and
   S3 printing. Assert results against independently specified synthetic fixtures.
4. Add the HTML viewer and saving workflow; refactor shared presentation only
   where it removes duplication without coupling the report calculations to UI.
5. Document the public workflow, support boundaries, and examples; update NEWS,
   package reference navigation, and any intentionally changed asset snapshots.

### Required regression scenarios

- Exact field-name validation, checkbox parent selection, duplicate/blank names,
  missing definitions, old codebook compatibility, and unavailable optional columns.
- Seed definitions retain calculations, raw annotation strings, labels/notes,
  section headers, descriptive fields, and optional columns. Source availability
  remains distinguishable from blank cells.
- A weight/height/BMI fixture has direct, indirect, and upstream relationships,
  multiple relationship types on one field, repeated occurrences, and unrelated
  sibling inputs. Each reported item has the expected expression and evidence path.
- Ordinary references, checkbox choices, explicit events, instances, modifiers,
  smart selectors, nested conditions, quoted literals, escaped quotes, whitespace,
  similarly named fields, unknown tags, and malformed/unsupported syntax.
- Quoted piping in `@DEFAULT` is discovered; plain bracket-like text in an ordinary
  formula literal is not. Ambiguous annotation expressions disclose uncertainty.
- Piping and embedding in labels, notes, headers, and choice labels retain their
  exact owning locations; project-setting consumers do not masquerade as fields.
- A cross-event reference to a real field can still have invalid context. Fields
  used in multiple events and repeating instruments preserve that distinction.
- No references, only incoming references, diamonds/multiple paths, disconnected
  subgraphs, self-references, and cycles have deterministic, complete output.
- Removal/value/definition concerns preserve calculation, default, visibility,
  piping, and embedding semantics. Counts distinguish occurrences from items.
- Extraction runs against the supplied codebook with no network requests or
  record retrieval; HTML-like metadata and expression text are never executed.
- Viewer filters, empty states, evidence details, escaping, multiple viewer roots,
  keyboard navigation, and standalone saved output behave correctly. Exercise
  browser interactions in an allowed environment; asset snapshots alone are
  insufficient acceptance evidence.

Run R work with `Rscript -e` and initialize package sessions with
`devtools::load_all()`. Run targeted and full testthat suites, `devtools::document()`,
lintr, non-rewriting styler verification, `R CMD check --no-manual` including
vignettes, and a pkgdown build. Use only dependencies listed in DESCRIPTION in
tests. Verify saved HTML offline and document any remaining browser-validation
limitation. The feature is complete when the weight example produces the expected
relationships and explanations, and existing package tests pass without
unexplained output or snapshot changes.
