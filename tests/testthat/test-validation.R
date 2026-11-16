test_that("integer and number validation preserve numeric syntax and reject invalid formats", {
  expect_equal(get_is_valid_validation_value(c("-12", "12.0"), "integer"), c(TRUE, FALSE))
  expect_equal(
    get_is_valid_validation_value(c("12.25", "1e3", ".850", "-.850", ".", "-."), "number"),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(get_parsed_validation_values(c(".850", "-.850"), "number"), c(0.85, -0.85))
  expect_equal(get_is_valid_validation_value(c("12,25", "12.25"), "number_comma_decimal"), c(TRUE, FALSE))
})

test_that("fixed precision validation enforces each supported decimal limit", {
  expect_equal(get_is_valid_validation_value(c("12.3", "12.34"), "number_1dp"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("12.34", "12.345"), "number_2dp"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c(".850", "12.3456"), "number_3dp"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("12.3456", "12.34567"), "number_4dp"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("12,3", "12,34"), "number_1dp_comma_decimal"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("12,34", "12,345"), "number_2dp_comma_decimal"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c(",850", "12,3456"), "number_3dp_comma_decimal"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("12,3456", "12,34567"), "number_4dp_comma_decimal"), c(TRUE, FALSE))
})

test_that("date validation uses raw ISO exports and rejects invalid calendar dates", {
  expect_equal(get_is_valid_validation_value(c("2025-12-31", "2025-02-30"), "date_dmy"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2025-12-31", "12-31-2025"), "date_mdy"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2024-02-29", "2025-02-29"), "date_ymd"), c(TRUE, FALSE))
  expect_equal(get_validation_expected("date_mdy"), "Valid REDCap date (YYYY-MM-DD)")
})

test_that("datetime validation enforces raw format and valid clock components", {
  expect_equal(get_is_valid_validation_value(c("2025-12-31 23:59", "2025-12-31 23:60"), "datetime_dmy"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2025-12-31 23:59", "12-31-2025 23:59"), "datetime_mdy"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2025-12-31 23:59", "2025-12-31 24:00"), "datetime_ymd"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2025-12-31 23:59:59", "2025-12-31 23:59"), "datetime_seconds_dmy"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2025-12-31 23:59:59", "2025-12-31 23:60:00"), "datetime_seconds_mdy"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("2025-12-31 23:59:59", "2025-13-31 23:59:59"), "datetime_seconds_ymd"), c(TRUE, FALSE))
  expect_equal(get_validation_expected("datetime_seconds_dmy"), "Valid REDCap datetime (YYYY-MM-DD HH:MM:SS)")
})

test_that("time validation respects hour minute and second ranges", {
  expect_equal(get_is_valid_validation_value(c("23:59", "24:00"), "time"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("23:59:59", "23:60:00"), "time_hh_mm_ss"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("59:59", "60:00"), "time_mm_ss"), c(TRUE, FALSE))
})

test_that("contact validation distinguishes supported email and phone formats", {
  expect_equal(get_is_valid_validation_value(c("person@example.org", "person@example"), "email"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("(215) 555-1212", "215-555-121"), "phone"), c(TRUE, FALSE))
  expect_equal(get_is_valid_validation_value(c("+61 412 345 678", "+61 12 345"), "phone_australia"), c(TRUE, FALSE))
})
