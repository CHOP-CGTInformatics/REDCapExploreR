get_test_metadata <- function(fields, forms = "main", types = "text") {
  tibble::tibble(field_name = fields, form_name = forms, field_type = types, field_label = fields)
}

get_test_quality_report <- function(
  data, metadata,
  checks = c("missingness", "metadata", "outliers", "operational", "consistency"),
  sparse_threshold = 0.95, outlier_iqr_multiplier = 3,
  evaluation_date = as.Date("2026-11-09"), ...
) {
  project <- get_quality_project_data(c(list(data = data, metadata = metadata), list(...)))
  get_quality_report(project, checks, sparse_threshold, outlier_iqr_multiplier, evaluation_date = evaluation_date)
}

get_test_codebook <- function(metadata, ...) {
  get_codebook(get_codebook_project(c(list(metadata = metadata), list(...))))
}

get_test_status_data <- function(data, metadata, ...) {
  get_record_status_tiles(get_quality_project_data(c(list(data = data, metadata = metadata), list(...))))
}
