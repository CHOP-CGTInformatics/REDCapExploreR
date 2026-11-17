# Changelog

## REDCapExploreR 0.1.0

- Identify referenced checkbox options in Summary using REDCap field
  notation, raw codes, and labels. Add Referenced choice to relationship
  viewer tables and clarify the source choice-label column as Containing
  choice code. Demonstrate Fever-specific branching in the bundled
  codebook and vignette.

- Rename the optional diagnostics viewer to Project Diagnostics and show
  only project-wide interpretation issues. Request
  `include_inspection_log = TRUE` separately for the complete inspection
  inventory and Full Inspection Log viewer.

- Make relationship report coverage identical for every field using the
  same codebook. Project Coverage includes all project interpretation
  limits; field-specific evidence remains in Issues.

- Handle REDCap’s explicit non-applicable event, form/event mapping, and
  arm responses for classic projects, and the response for projects
  without repeats. Codebooks, record-status data, and quality reports
  retain genuine API errors while supporting classic projects with or
  without repeating instruments.

- Separate field-name matching from qualifier interpretation; known
  references remain linked when qualifiers need review. Support combined
  checkbox piping modifiers and flag cross-instrument embedding without
  hiding its reference.

- Preserve metadata locations, expressions, action tags, and
  repeated-occurrence counts in relationship summaries. Label
  reference-chain steps without implying value propagation or
  event/instance continuity.

- Include related interpretation issues and original evidence by
  default; keep Coverage project-wide and full diagnostics optional.
  Simplify viewer tables, use REDCap terminology, and demonstrate
  realistic weight/BMI relationships.

- Expanded `mock_codebook` with upstream and downstream relationship
  examples, including indirect paths and interpretation notes. Viewer
  sections now use the same Field, Upstream, Downstream, Summary, and
  Coverage terms as the R report.

- Add
  [`field_relationship_summary()`](https://chop-cgtinformatics.github.io/REDCapExploreR/reference/field_relationship_summary.md)
  with complete definitions, upstream and downstream connections,
  original reference evidence, neutral explanations, and explicit source
  coverage. No record exports or expression evaluation are required;
  rebuild older codebooks to include complete definitions.

- Show all relationship references by default; readable relationship
  types and interpretation notes replace routine status codes in the
  viewer. R output retains the machine-readable type and resolution
  columns.

- Summarize coverage by source and relevant interpretation notes, with
  bounded examples and aggregate limits elsewhere. Set
  `include_diagnostics = TRUE` to retain full project-wide diagnostics
  and expose the optional diagnostics tab.

- Add searchable field-relationship viewers and standalone HTML saving.
  Scope viewer navigation to each root and support keyboard tab
  navigation.

- Codebooks now retain complete standardized definitions and source
  availability, including canonical repeating-event keys.

- Reject unsuccessful API responses instead of treating them as empty
  structure; retain recognized classic-project event responses and
  redact credentials from endpoint diagnostics.

- Handle successful zero-column empty record exports and require the
  actual metadata-defined record identifier for nonempty data.

- Preserve gray statuses as blank exports, resolve repeating instruments
  per event, and keep dashboard tiles distinct when display labels
  collide.

- Evaluate numeric branching numerically, preserve quoted literals and
  exact categorical codes, and support explicit blank comparisons.
  Unsupported REDCap functions, smart variables, arithmetic, and
  event-qualified expressions remain assessable rather than suppressing
  required-field findings.

- Restrict IQR calculations to structurally eligible finite observations
  and warn when unvalidated nonfinite values are excluded. Checkbox
  exclusivity follows metadata labels rather than legacy code suffixes.

- Add report settings and missingness assessment coverage; clarify
  sparse-field denominators, completion percentages, and free-text
  review suggestions.

- Consolidate instrument normalization, form structure, and completion
  extraction; separate retrieval, normalization, applicability,
  branching, and progress code.

- Existing function signatures, finding identifiers, and summary column
  names are retained. Instrument metadata preserves original columns and
  adds standardized names; dashboard labels change only when needed to
  disambiguate tiles.
