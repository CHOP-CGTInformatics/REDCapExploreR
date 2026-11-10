get_weight_codebook <- function() {
  metadata <- tibble::tibble(
    field_name = c(
      "record_id", "weight_kg", "height_cm", "bmi", "review_required",
      "review_notes", "starting_weight", "visit_summary", "panel", "symptoms"
    ),
    form_name = "main",
    field_type = c("text", "text", "text", "calc", "calc", "text", "text", "descriptive", "descriptive", "checkbox"),
    field_label = c(
      "Record", "Weight", "Height", "BMI", "Review", "Notes", "Starting weight",
      "Weight: [weight_kg] kg", "{weight_kg}", "Symptoms"
    ),
    select_choices_or_calculations = c(
      NA, NA, NA, "[weight_kg] / ([height_cm]/100)^2",
      "[bmi] > 30", NA, NA, NA, NA, "1, Fever | 2, Rash"
    ),
    branching_logic = c(NA, "[record_id] <> ''", NA, NA, NA, "[review_required] = 1", NA, NA, NA, NA),
    field_annotation = c(NA, "@READONLY", NA, NA, NA, NA, "@DEFAULT='[weight_kg]'", NA, NA, NA)
  )
  get_codebook(get_codebook_project(list(
    metadata = metadata,
    events = tibble::tibble(unique_event_name = c("baseline_arm_1", "followup_arm_1"), event_name = c("Baseline", "Followup")),
    event_instruments = tibble::tibble(unique_event_name = c("baseline_arm_1", "followup_arm_1"), form = "main"),
    repeating_instruments = tibble::tibble(event_name = "followup_arm_1", form_name = "main")
  )))
}
