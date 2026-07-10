devtools::load_all(".")

mock_redcap_project <- list(
  data = tibble::tibble(
    record_id = c(
      "C001", "C001", "C001",
      "C002", "C002", "C002",
      "C0003", "C0003", "C0003", "C0003"
    ),
    redcap_event_name = c(
      "baseline_arm_1",
      "month_1_arm_1",
      "month_1_arm_1",
      "baseline_arm_1",
      "month_1_arm_1",
      "month_1_arm_1",
      "baseline_arm_1",
      "month_1_arm_1",
      "month_1_arm_1",
      "month_1_arm_1"
    ),
    redcap_repeat_instrument = c(
      NA,
      NA,
      "adverse_events",
      NA,
      NA,
      "adverse_events",
      NA,
      "adverse_events",
      "adverse_events",
      "adverse_events"
    ),
    redcap_repeat_instance = c(NA, NA, 1, NA, NA, 1, NA, 1, 2, 3),
    age = c(45, NA, NA, 999, NA, NA, 61, NA, NA, NA),
    enrollment_date = c(
      "2025-01-15",
      NA,
      NA,
      "2999-01-01",
      NA,
      NA,
      "2025-03-10",
      NA,
      NA,
      NA
    ),
    sex = c("1", NA, NA, "2", NA, NA, "3", NA, NA, NA),
    visit_date = c(
      NA,
      "2025-02-15",
      NA,
      NA,
      "",
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    response_score = c(NA, 8, NA, NA, NA, NA, NA, NA, NA, NA),
    visit_notes = c(
      NA,
      "Mild fatigue after treatment.",
      NA,
      NA,
      "",
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    symptoms___none = c(NA, 0, NA, NA, 1, NA, NA, NA, NA, NA),
    symptoms___fatigue = c(NA, 1, NA, NA, 1, NA, NA, NA, NA, NA),
    symptoms___fever = c(NA, 0, NA, NA, 0, NA, NA, NA, NA, NA),
    ae_term = c(
      NA,
      NA,
      "Fatigue",
      NA,
      NA,
      "Headache",
      NA,
      "Nausea",
      "Rash",
      "Fever"
    ),
    ae_grade = c(NA, NA, 2, NA, NA, 1, NA, 1, 2, 3),
    ae_related = c(NA, NA, "1", NA, NA, "0", NA, "1", "1", "0"),
    demographics_complete = c(2, NA, NA, 2, NA, NA, 0, NA, NA, NA),
    follow_up_complete = c(NA, 2, NA, NA, 0, NA, NA, NA, NA, NA),
    adverse_events_complete = c(NA, NA, 2, NA, NA, 1, NA, 2, 2, 0)
  ),
  metadata = tibble::tibble(
    field_name = c(
      "record_id",
      "age",
      "enrollment_date",
      "sex",
      "visit_date",
      "response_score",
      "visit_notes",
      "symptoms",
      "ae_term",
      "ae_grade",
      "ae_related"
    ),
    form_name = c(
      rep("demographics", 4),
      rep("follow_up", 4),
      rep("adverse_events", 3)
    ),
    field_type = c(
      "text",
      "text",
      "text",
      "radio",
      "text",
      "text",
      "notes",
      "checkbox",
      "text",
      "text",
      "radio"
    ),
    field_label = c(
      "Record ID",
      "Age at enrollment",
      "Enrollment date",
      "Sex",
      "Visit date",
      "Response score",
      "Visit notes",
      "Symptoms",
      "Adverse event term",
      "Adverse event grade",
      "Related to treatment"
    ),
    select_choices_or_calculations = c(
      NA,
      NA,
      NA,
      "1, Female | 2, Male | 3, Unknown",
      NA,
      NA,
      NA,
      "none, None | fatigue, Fatigue | fever, Fever",
      NA,
      NA,
      "1, Yes | 0, No"
    ),
    text_validation_type_or_show_slider_number = c(
      NA,
      "integer",
      "date_ymd",
      NA,
      "date_ymd",
      "integer",
      NA,
      NA,
      NA,
      "integer",
      NA
    ),
    text_validation_min = c(NA, 0, NA, NA, NA, 0, NA, NA, NA, 1, NA),
    text_validation_max = c(NA, 120, NA, NA, NA, 10, NA, NA, NA, 5, NA),
    required_field = c("y", "y", "y", "n", "y", "n", "n", "n", "y", "y", "n"),
    branching_logic = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "[response_score] < '10'",
      NA,
      NA,
      NA
    ),
    identifier = c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n")
  ),
  events = tibble::tibble(
    event_name = c("Baseline", "Month 1"),
    arm_num = c(1L, 1L),
    unique_event_name = c("baseline_arm_1", "month_1_arm_1")
  ),
  event_instruments = tibble::tibble(
    unique_event_name = c(
      "baseline_arm_1",
      "month_1_arm_1",
      "month_1_arm_1"
    ),
    instrument = c("demographics", "follow_up", "adverse_events"),
    arm_num = c(1L, 1L, 1L)
  ),
  instruments = tibble::tibble(
    instrument_name = c("demographics", "follow_up", "adverse_events"),
    instrument_label = c("Demographics", "Follow-up", "Adverse Events")
  ),
  repeating_instruments = tibble::tibble(
    event_name = "month_1_arm_1",
    instrument_name = "adverse_events",
    unique_event_name = "month_1_arm_1"
  ),
  project_info = tibble::tibble(
    project_id = 1001,
    project_title = "Mock REDCap Database",
    in_production = FALSE,
    project_language = "English",
    surveys_enabled = FALSE,
    is_longitudinal = TRUE,
    has_repeating_instruments_or_events = TRUE
  )
)

mock_record_status_data <- mock_redcap_project |>
  get_quality_project_data() |>
  get_record_status_tiles()

mock_codebook <- mock_redcap_project |>
  get_codebook_project() |>
  get_codebook()

mock_quality_report <- mock_redcap_project |>
  get_quality_project_data() |>
  get_quality_report(
    checks = c(
      "missingness",
      "metadata",
      "outliers",
      "operational",
      "consistency"
    ),
    sparse_threshold = 0.95,
    outlier_iqr_multiplier = 3
  )

usethis::use_data(
  mock_redcap_project,
  mock_record_status_data,
  mock_codebook,
  mock_quality_report,
  overwrite = TRUE
)
