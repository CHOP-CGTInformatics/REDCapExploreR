#' @title Determine which rows satisfy supported REDCap branching logic
#'
#' @description Evaluates comparisons and Boolean expressions without executing
#'   REDCap metadata as R code. Unsupported expressions remain assessable.
#' @param project A normalized REDCap project.
#' @param logic One branching expression.
#' @returns One logical applicability value per exported row.
#' @noRd
get_branching_applicable_rows <- function(project, logic) {
  if (length(logic) == 0 || get_is_missing(logic)) {
    return(rep(TRUE, nrow(project$data)))
  }
  out <- tryCatch(
    get_evaluated_branching_logic(project, logic),
    error = function(cnd) rep(TRUE, nrow(project$data))
  )
  out[is.na(out)] <- FALSE
  out
}

#' @title Tokenize supported REDCap branching logic
#' @description Preserves quoted literals while identifying supported tokens.
#' @param logic One branching expression.
#' @returns Character tokens including their original quoting.
#' @noRd
get_branching_tokens <- function(logic) {
  pattern <- paste0(
    r"{\s*(?:'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"|}",
    r"{\[[A-Za-z][A-Za-z0-9_]*(?:\([^][()]+\))?\]|}",
    r"{<=|>=|<>|!=|==|=|<|>|\(|\)|(?i:\band\b|\bor\b)|}",
    r"{[+-]?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+))}"
  )
  logic <- trimws(logic)
  tokens <- regmatches(logic, gregexpr(pattern, logic, perl = TRUE))[[1]]
  if (length(tokens) == 0 || paste0(tokens, collapse = "") != logic) {
    stop("Unsupported branching syntax.")
  }
  trimws(tokens)
}

#' @title Evaluate a branching expression using recursive descent
#' @description Compares typed operands and combines logical evaluations.
#' @param project A normalized REDCap project.
#' @param logic One branching expression.
#' @returns One logical evaluation per exported row.
#' @noRd
get_evaluated_branching_logic <- function(project, logic) {
  tokens <- get_branching_tokens(logic)
  state <- new.env(parent = emptyenv())
  state$position <- 1L
  peek <- function() {
    if (state$position > length(tokens)) "" else tolower(tokens[[state$position]])
  }
  consume <- function() {
    if (state$position > length(tokens)) stop("Incomplete branching expression.")
    token <- tokens[[state$position]]
    state$position <- state$position + 1L
    token
  }
  operand <- function() {
    token <- consume()
    if (startsWith(token, "[")) {
      field <- gsub("^\\[|\\]$", "", token)
      field <- gsub("\\(([^)]+)\\)$", "___\\1", field)
      if (!field %in% names(project$data)) stop("Unavailable branching reference.")
      value <- as.character(project$data[[field]])
      value[get_is_missing(value)] <- NA_character_
      raw_value <- value
      metadata <- get_metadata_row(project, sub("___.*$", "", field))
      validation <- tolower(metadata[["text_validation_type_or_show_slider_number"]])
      numeric <- length(validation) == 1 &&
        validation %in% get_supported_text_validations() &&
        get_validation_family(validation) == "number" &&
        metadata$field_type == "text"
      if (numeric) {
        parsed <- get_parsed_validation_values(value, validation)
        value <- as.character(parsed)
      }
      return(list(
        value = value, raw_value = raw_value, numeric = numeric, categorical =
          metadata$field_type %in% c("checkbox", "dropdown", "radio", "yesno", "truefalse")
      ))
    }
    quoted <- startsWith(token, "'") || startsWith(token, '"')
    if (quoted) {
      # Parsing is limited to a single lexer-validated string literal.
      value <- parse(text = token)[[1]]
    } else {
      value <- as.numeric(token)
    }
    list(value = value, raw_value = value, numeric = !quoted, categorical = FALSE)
  }
  comparison <- function() {
    if (peek() == "(") {
      consume()
      value <- disjunction()
      if (consume() != ")") stop("Unclosed branching group.")
      return(value)
    }
    left <- operand()
    operator <- consume()
    if (!operator %in% c("=", "==", "!=", "<>", "<", "<=", ">", ">=")) {
      stop("Unsupported branching operator.")
    }
    right <- operand()
    blank_test <- (length(left$value) == 1 && !is.na(left$value) && left$value == "") ||
      (length(right$value) == 1 && !is.na(right$value) && right$value == "")
    numeric <- (left$numeric || right$numeric) && !blank_test
    if (operator %in% c("=", "==", "!=", "<>") &&
      (left$categorical || right$categorical)) {
      numeric <- FALSE
    }
    if (numeric) {
      left_value <- suppressWarnings(as.numeric(left$value))
      right_value <- suppressWarnings(as.numeric(right$value))
    } else {
      left_value <- as.character(if (blank_test) left$raw_value else left$value)
      right_value <- as.character(if (blank_test) right$raw_value else right$value)
      if (blank_test) {
        left_value[is.na(left_value)] <- ""
        right_value[is.na(right_value)] <- ""
      }
    }
    out <- switch(operator,
      "=" = left_value == right_value,
      "==" = left_value == right_value,
      "!=" = left_value != right_value,
      "<>" = left_value != right_value,
      "<" = left_value < right_value,
      "<=" = left_value <= right_value,
      ">" = left_value > right_value,
      ">=" = left_value >= right_value
    )
    rep_len(out, nrow(project$data))
  }
  conjunction <- function() {
    value <- comparison()
    while (peek() == "and") {
      consume()
      value <- value & comparison()
    }
    value
  }
  disjunction <- function() {
    value <- conjunction()
    while (peek() == "or") {
      consume()
      value <- value | conjunction()
    }
    value
  }
  out <- disjunction()
  if (state$position <= length(tokens)) stop("Unsupported branching expression.")
  out
}

get_branching_references <- function(logic) {
  if (length(logic) == 0 || get_is_missing(logic)) {
    return(character())
  }

  pattern <- r"{'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"|\[[^]]+\]}"
  matches <- gregexpr(pattern, logic, perl = TRUE)
  refs <- regmatches(logic, matches)[[1]]
  refs <- refs[startsWith(refs, "[")]

  if (length(refs) == 0 || refs[[1]] == "-1") {
    return(character())
  }

  refs <- gsub("^\\[|\\]$", "", refs)
  refs <- gsub("\\(.*\\)$", "", refs)
  unique(refs)
}
