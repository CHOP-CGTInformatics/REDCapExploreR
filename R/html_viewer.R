get_validate_page_length <- function(page_length) {
  valid_page_length <- is.numeric(page_length) &&
    length(page_length) == 1 &&
    !is.na(page_length) &&
    page_length > 0 &&
    page_length == as.integer(page_length)

  if (!valid_page_length) {
    cli_abort("{.arg page_length} must be a positive integer.")
  }

  invisible(NULL)
}

get_validate_html_file <- function(file) {
  valid_file <- is.character(file) &&
    length(file) == 1 &&
    !is.na(file) &&
    file != ""

  if (!valid_file) {
    cli_abort("{.arg file} must be a single non-empty HTML file path.")
  }

  parent_dir <- dirname(file)
  if (!dir.exists(parent_dir)) {
    cli_abort("The output directory for {.arg file} must already exist.")
  }

  invisible(NULL)
}

get_save_html_single_file <- function(viewer, file, source) {
  temp_dir <- tempfile("redcap-html-save-")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  temp_file <- file.path(temp_dir, basename(file))
  save_html(viewer, file = temp_file, libdir = "lib")

  html <- paste(readLines(temp_file, warn = FALSE), collapse = "\n")
  html <- get_inline_html_styles(html, temp_dir, source)
  html <- get_inline_html_scripts(html, temp_dir, source)

  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

get_inline_html_styles <- function(html, base_dir, source) {
  get_inline_html_dependency(
    html = html,
    base_dir = base_dir,
    pattern = "<link([^>]+)href=\"([^\"]+)\"([^>]*)>",
    replacement = \(path, content) {
      paste0(
        "<style data-",
        source,
        "-source=\"",
        path,
        "\">\n",
        content,
        "\n</style>"
      )
    }
  )
}

get_inline_html_scripts <- function(html, base_dir, source) {
  get_inline_html_dependency(
    html = html,
    base_dir = base_dir,
    pattern = "<script([^>]*)src=\"([^\"]+)\"([^>]*)></script>",
    replacement = \(path, content) {
      paste0(
        "<script data-",
        source,
        "-source=\"",
        path,
        "\">\n",
        content,
        "\n</script>"
      )
    }
  )
}

get_inline_html_dependency <- function(html, base_dir, pattern, replacement) {
  matches <- gregexpr(pattern, html, perl = TRUE)
  starts <- matches[[1]]
  if (starts[[1]] == -1) {
    return(html)
  }

  match_text <- regmatches(html, matches)[[1]]
  paths <- map_chr(match_text, \(match) {
    sub(pattern, "\\2", match, perl = TRUE)
  })

  for (index in rev(seq_along(match_text))) {
    path <- paths[[index]]
    local_file <- file.path(base_dir, path)
    if (!file.exists(local_file)) {
      next
    }

    content <- paste(readLines(local_file, warn = FALSE), collapse = "\n")
    inline <- replacement(path, content)
    start <- starts[[index]]
    end <- start + attr(starts, "match.length")[[index]] - 1
    html <- paste0(
      substr(html, 1, start - 1),
      inline,
      substr(html, end + 1, nchar(html))
    )
  }

  html
}
