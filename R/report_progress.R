get_progress_enabled <- function(progress) {
  if (progress == "show") {
    return(TRUE)
  }

  if (progress == "none") {
    return(FALSE)
  }

  interactive()
}

get_report_progress_steps <- function(checks) {
  length(checks) + 6L
}

get_progress_bar <- function(enabled, total_steps) {
  if (!enabled) {
    return(NULL)
  }

  progress_env <- new.env(parent = parent.frame())
  progress_id <- cli_progress_bar(
    name = "Building REDCap quality report",
    total = total_steps,
    format = "{cli::pb_name}{cli::pb_bar} {cli::pb_percent} | {cli::pb_status}",
    format_done = paste0(
      "{cli::pb_name}{cli::pb_percent} | ",
      symbol$tick,
      " Complete"
    ),
    clear = FALSE,
    auto_terminate = FALSE,
    .envir = progress_env
  )
  list(id = progress_id, envir = progress_env)
}

update_report_progress <- function(progress_bar, enabled, force, message) {
  if (!enabled) {
    return(invisible(NULL))
  }

  tryCatch(
    cli_progress_update(
      id = progress_bar$id,
      inc = 1,
      status = message,
      force = force,
      .envir = progress_bar$envir
    ),
    error = function(cnd) NULL
  )
  invisible(NULL)
}

close_report_progress <- function(progress_bar, enabled) {
  if (!enabled) {
    return(invisible(NULL))
  }

  tryCatch(
    cli_progress_done(id = progress_bar$id, .envir = progress_bar$envir),
    error = function(cnd) NULL
  )
  invisible(NULL)
}
