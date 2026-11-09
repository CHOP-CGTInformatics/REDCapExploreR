# Changelog

## REDCapExploreR 0.1.0

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
