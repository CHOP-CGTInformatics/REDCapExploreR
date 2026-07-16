#' Parse REDCap text validation values
#'
#' @description
#' Provides strict, metadata-driven parsing for supported REDCap text
#' validation types.
#'
#' @param value Exported REDCap field values.
#' @param validation REDCap text validation type.
#'
#' @returns Parsed numeric values, with invalid or missing values represented by
#'   `NA_real_`.
#'
#' @noRd
get_parsed_validation_values <- function(value, validation) {
  value <- as.character(value)
  validation <- tolower(validation)
  family <- get_validation_family(validation)
  missing <- get_is_missing(value)

  if (family == "number") {
    valid <- !missing &
      grepl(
        get_number_validation_pattern(validation),
        value,
        perl = TRUE
      )
    if (str_detect(validation, "comma_decimal$")) {
      value[valid] <- sub(",", ".", value[valid], fixed = TRUE)
    }

    parsed <- rep(NA_real_, length(value))
    parsed[valid] <- as.numeric(value[valid])
    return(parsed)
  }

  if (family %in% c("date", "datetime")) {
    return(get_parsed_temporal_values(value, validation, family))
  }

  if (family == "time") {
    return(map_dbl(value, \(item) get_parsed_time_value(item, validation)))
  }

  rep(NA_real_, length(value))
}

get_parsed_validation_value <- function(value, validation) {
  get_parsed_validation_values(value, validation)[[1]]
}

get_cached_validation_values <- function(project, field, validation) {
  if (!"validation_cache" %in% names(project)) {
    return(get_parsed_validation_values(project$data[[field]], validation))
  }

  key <- paste(field, validation, sep = "::")
  if (!exists(key, envir = project$validation_cache, inherits = FALSE)) {
    project$validation_cache[[key]] <- get_parsed_validation_values(
      project$data[[field]],
      validation
    )
  }

  project$validation_cache[[key]]
}

get_is_valid_validation_value <- function(value, validation) {
  value <- as.character(value)
  validation <- tolower(validation)
  family <- get_validation_family(validation)

  if (family %in% c("number", "date", "datetime", "time")) {
    return(
      get_is_missing(value) |
        !is.na(get_parsed_validation_values(value, validation))
    )
  }

  if (family == "email") {
    return(
      get_is_missing(value) |
        grepl(
          "^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",
          value,
          perl = TRUE
        )
    )
  }

  if (family == "phone") {
    return(
      get_is_missing(value) |
        map_lgl(value, \(item) get_is_valid_phone(item, validation))
    )
  }

  rep(TRUE, length(value))
}

get_supported_text_validations <- function() {
  c(
    "integer",
    "number",
    "number_comma_decimal",
    paste0("number_", 1:4, "dp"),
    paste0("number_", 1:4, "dp_comma_decimal"),
    "date_dmy",
    "date_mdy",
    "date_ymd",
    "datetime_dmy",
    "datetime_mdy",
    "datetime_ymd",
    "datetime_seconds_dmy",
    "datetime_seconds_mdy",
    "datetime_seconds_ymd",
    "time",
    "time_hh_mm_ss",
    "time_mm_ss",
    "email",
    "phone",
    "phone_australia"
  )
}

get_bounded_text_validations <- function() {
  validations <- get_supported_text_validations()
  validations[map_lgl(validations, \(validation) {
    get_validation_family(validation) %in%
      c("number", "date", "datetime", "time")
  })]
}

get_validation_family <- function(validation) {
  case_when(
    validation == "integer" | startsWith(validation, "number") ~ "number",
    startsWith(validation, "datetime") ~ "datetime",
    startsWith(validation, "date") ~ "date",
    startsWith(validation, "time") ~ "time",
    validation == "email" ~ "email",
    startsWith(validation, "phone") ~ "phone",
    TRUE ~ "unsupported"
  )
}

get_number_validation_pattern <- function(validation) {
  if (validation == "integer") {
    return("^-?[0-9]+$")
  }

  decimal_mark <- if (str_detect(validation, "comma_decimal$")) "," else "\\."
  precision <- suppressWarnings(as.integer(sub(
    "^number_([1-4])dp.*$",
    "\\1",
    validation
  )))
  decimals <- if (is.na(precision)) "+" else paste0("{1,", precision, "}")

  paste0(
    "^-?(?:[0-9]+(?:",
    decimal_mark,
    "[0-9]",
    decimals,
    ")?|",
    decimal_mark,
    "[0-9]",
    decimals,
    ")$"
  )
}

get_temporal_validation_format <- function(validation) {
  if (startsWith(validation, "datetime_seconds")) {
    return("%Y-%m-%d %H:%M:%S")
  }
  if (startsWith(validation, "datetime")) {
    return("%Y-%m-%d %H:%M")
  }

  "%Y-%m-%d"
}

get_parsed_temporal_value <- function(value, validation, family) {
  get_parsed_temporal_values(value, validation, family)[[1]]
}

get_parsed_temporal_values <- function(value, validation, family) {
  format <- get_temporal_validation_format(validation)
  parsed <- if (family == "date") {
    suppressWarnings(as.Date(value, format = format))
  } else {
    suppressWarnings(as.POSIXct(strptime(value, format = format, tz = "UTC")))
  }

  valid <- !get_is_missing(value) &
    !is.na(parsed) &
    format(parsed, format, tz = "UTC") == value

  out <- as.numeric(parsed)
  out[!valid] <- NA_real_
  out
}

get_parsed_time_value <- function(value, validation) {
  patterns <- c(
    time = "^[0-9]{2}:[0-9]{2}$",
    time_hh_mm_ss = "^[0-9]{2}:[0-9]{2}:[0-9]{2}$",
    time_mm_ss = "^[0-9]{2}:[0-9]{2}$"
  )
  pattern <- patterns[[validation]]
  if (!grepl(pattern, value)) {
    return(NA_real_)
  }

  parts <- as.numeric(strsplit(value, ":", fixed = TRUE)[[1]])
  if (validation == "time_mm_ss") {
    if (parts[[1]] > 59 || parts[[2]] > 59) {
      return(NA_real_)
    }
    return(parts[[1]] * 60 + parts[[2]])
  }

  if (parts[[1]] > 23 || parts[[2]] > 59) {
    return(NA_real_)
  }
  if (length(parts) == 3 && parts[[3]] > 59) {
    return(NA_real_)
  }

  hours <- parts[[1]] * 3600
  minutes <- parts[[2]] * 60
  seconds <- ifelse(length(parts) == 3, parts[[3]], 0)
  hours + minutes + seconds
}

get_is_valid_phone <- function(value, validation) {
  if (trimws(value) != value || !grepl("^\\+?[0-9() .-]+$", value)) {
    return(FALSE)
  }

  digits <- gsub("[^0-9]", "", value)
  has_plus <- startsWith(value, "+")
  if (validation == "phone") {
    international <- nchar(digits) == 11 && startsWith(digits, "1")
    return((!has_plus && nchar(digits) == 10) || international)
  }

  local <- !has_plus && nchar(digits) == 10 && startsWith(digits, "0")
  international <- nchar(digits) == 11 && startsWith(digits, "61")
  local || international
}

get_invalid_validation_findings <- function(project) {
  metadata <- project$metadata |>
    filter(
      tolower(.data$field_type) == "text",
      .data$field_name %in% names(project$data),
      tolower(.data$text_validation_type_or_show_slider_number) %in%
        get_supported_text_validations()
    )

  bind_rows(map(seq_len(nrow(metadata)), \(index) {
    field <- metadata$field_name[[index]]
    validation <- tolower(
      metadata$text_validation_type_or_show_slider_number[[index]]
    )
    value <- as.character(project$data[[field]])
    applicable <- get_form_applicable_rows(
      project,
      metadata$form_name[[index]]
    )
    family <- get_validation_family(validation)
    valid <- if (family %in% c("number", "date", "datetime", "time")) {
      get_is_missing(value) |
        !is.na(get_cached_validation_values(project, field, validation))
    } else {
      get_is_valid_validation_value(value, validation)
    }
    invalid <- applicable &
      !get_is_missing(value) &
      !valid

    if (!any(invalid)) {
      return(get_empty_findings())
    }

    tibble(
      check = "consistency",
      issue = "invalid_validation_format",
      severity = "warning",
      scope = "record_field",
      record_id = as.character(project$data[[project$record_id_field]][
        invalid
      ]),
      form_name = metadata$form_name[[index]],
      event_name = get_event_values(project$data, invalid),
      repeat_instrument = get_repeat_instrument_values(project$data, invalid),
      repeat_instance = get_repeat_instance_values(project$data, invalid),
      field_name = field,
      value = value[invalid],
      expected = get_validation_expected(validation),
      message = paste(
        "Field",
        field,
        "does not match its REDCap text validation."
      )
    )
  }))
}

get_validation_expected <- function(validation) {
  family <- get_validation_family(validation)
  if (family == "date") {
    return("Valid REDCap date (YYYY-MM-DD)")
  }
  if (family == "datetime") {
    format <- if (startsWith(validation, "datetime_seconds")) {
      "YYYY-MM-DD HH:MM:SS"
    } else {
      "YYYY-MM-DD HH:MM"
    }
    return(paste("Valid REDCap datetime", paste0("(", format, ")")))
  }

  paste("Value matching REDCap", validation, "validation")
}

get_validation_date_values <- function(value, validation) {
  get_validation_dates_from_parsed(
    get_parsed_validation_values(value, validation),
    validation
  )
}

get_validation_dates_from_parsed <- function(parsed, validation) {
  family <- get_validation_family(validation)

  if (family == "date") {
    return(as.Date(parsed, origin = "1970-01-01"))
  }

  as.Date(as.POSIXct(parsed, origin = "1970-01-01", tz = "UTC"))
}
