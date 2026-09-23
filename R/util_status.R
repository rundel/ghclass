#' @name with_progress
#' @rdname with_progress
#'
#' @title Progress mode for status messages
#'
#' @description
#' By default ghclass functions that act on many repositories, teams or users
#' print one line per item. Progress mode replaces the per-item success
#' messages with a progress bar that shows the most recently completed step
#' and prints a single summary line when the operation finishes. Failures are
#' still reported in full as they happen.
#'
#' Progress mode is enabled by setting `options(ghclass.progress = TRUE)` or
#' by wrapping code in `with_progress()`. Functions that do not yet support
#' progress mode print their usual per-item messages.
#'
#' @param code Code to execute with progress mode enabled.
#'
#' @return The result of evaluating `code`.
#'
#' @examples
#' \dontrun{
#' with_progress(
#'   org_create_assignment("ghclass-test", repo = "hw1", user = c("a", "b"))
#' )
#' }
#'
#' @export
with_progress = function(code) {
  withr::local_options(list(ghclass.progress = TRUE))
  res = withVisible(code)

  if (res[["visible"]]) res[["value"]] else invisible(res[["value"]])
}

# Stack of active progress scopes, innermost last
status_env = new.env(parent = emptyenv())
status_env[["scopes"]] = list()

progress_enabled = function() {
  isTRUE(getOption("ghclass.progress", FALSE))
}

# An event belongs to the innermost scope whose owner function lexically
# encloses the reporting call, so helpers called from inside a loop that
# report on their own (e.g. repo_branches()) are not counted by the loop.
status_scope_for = function(envir) {
  scopes = status_env[["scopes"]]

  for (i in rev(seq_along(scopes))) {
    if (env_encloses(scopes[[i]][["envir"]], envir))
      return(scopes[[i]])
  }

  NULL
}

env_encloses = function(owner, envir) {
  e = envir
  while (!identical(e, emptyenv())) {
    if (identical(e, owner))
      return(TRUE)
    if (isNamespace(e) || identical(e, globalenv()))
      return(FALSE)
    e = parent.env(e)
  }

  FALSE
}

# Runs a loop that reports through status_msg(), status_skip(), status_fail()
# and status_note() as a single progress bar with a summary line.
status_scope = function(name, total, expr, done = NULL) {
  if (!progress_enabled() || total == 0)
    return(expr)

  scope = new.env(parent = emptyenv())
  scope[["name"]] = name
  scope[["total"]] = total
  scope[["done"]] = if (is.null(done)) "{n_ok} of {total} succeeded" else done
  scope[["n_ok"]] = 0L
  scope[["n_fail"]] = 0L
  scope[["n_skip"]] = 0L
  scope[["state"]] = "running"
  scope[["depth"]] = length(status_env[["scopes"]]) + 1L
  scope[["envir"]] = parent.frame()

  status_env[["scopes"]][[scope[["depth"]]]] = scope
  on.exit(status_scope_pop(scope), add = TRUE)

  scope[["bar"]] = cli::cli_progress_bar(
    name = name, total = total,
    format = paste(
      "{cli::pb_spin} {cli::pb_name} {cli::pb_bar}",
      "{cli::pb_current}/{cli::pb_total} | {cli::pb_status}"
    ),
    auto_terminate = FALSE, clear = TRUE,
    .envir = environment()
  )

  res = withCallingHandlers(
    expr,
    error = function(e) scope[["state"]] = "error",
    interrupt = function(e) scope[["state"]] = "interrupt"
  )
  scope[["state"]] = "completed"

  res
}

status_scope_pop = function(scope) {
  status_env[["scopes"]] = status_env[["scopes"]][seq_len(scope[["depth"]] - 1L)]

  tryCatch(
    cli::cli_progress_done(id = scope[["bar"]], result = "clear"),
    error = function(e) NULL
  )

  status_scope_summary(scope)
}

status_scope_summary = function(scope) {
  env = list2env(
    mget(c("name", "total", "n_ok", "n_fail", "n_skip"), envir = scope),
    parent = scope[["envir"]]
  )
  env[["n_done"]] = env[["n_ok"]] + env[["n_fail"]] + env[["n_skip"]]

  if (scope[["state"]] == "completed") {
    msg = scope[["done"]]
    if (env[["n_fail"]] > 0)
      msg = paste0(msg, ", {n_fail} failed")
    if (env[["n_skip"]] > 0)
      msg = paste0(msg, ", {n_skip} skipped")

    if (env[["n_fail"]] > 0)
      cli::cli_alert_danger(msg, wrap = FALSE, .envir = env)
    else if (env[["n_ok"]] > 0)
      cli::cli_alert_success(msg, wrap = FALSE, .envir = env)
    else
      cli::cli_alert_info(msg, wrap = FALSE, .envir = env)
  } else {
    counts = c(
      if (env[["n_ok"]] > 0) "{n_ok} succeeded",
      if (env[["n_fail"]] > 0) "{n_fail} failed",
      if (env[["n_skip"]] > 0) "{n_skip} skipped"
    )
    msg = "{name} aborted after {n_done} of {total}"
    if (length(counts) > 0)
      msg = paste0(msg, ": ", paste(counts, collapse = ", "))

    cli::cli_alert_danger(msg, wrap = FALSE, .envir = env)
  }

  invisible(NULL)
}

status_scope_event = function(scope, outcome, msg = NULL, n = 1L) {
  field = switch(outcome, ok = "n_ok", fail = "n_fail", skip = "n_skip")
  scope[[field]] = scope[[field]] + n

  cli::cli_progress_update(id = scope[["bar"]], inc = n, status = msg)

  invisible(TRUE)
}

status_text = function(msg, .envir) {
  cli::format_inline(msg, .envir = .envir, collapse = TRUE)
}

# Reports an item that was skipped before any API call
status_skip = function(msg, n = 1L, .envir = parent.frame()) {
  scope = status_scope_for(.envir)

  if (is.null(scope))
    cli::cli_alert_info(msg, wrap = FALSE, .envir = .envir)
  else
    status_scope_event(scope, "skip", status_text(msg, .envir), n = n)

  invisible(NULL)
}

# Reports a failure detected before any API call
status_fail = function(msg, .envir = parent.frame()) {
  scope = status_scope_for(.envir)

  cli::cli_alert_danger(msg, wrap = FALSE, .envir = .envir)
  if (!is.null(scope))
    status_scope_event(scope, "fail", status_text(msg, .envir))

  invisible(NULL)
}

# Reports progress within an item without counting an outcome
status_note = function(msg, .envir = parent.frame()) {
  scope = status_scope_for(.envir)

  if (is.null(scope))
    cli::cli_alert_success(msg, wrap = FALSE, .envir = .envir)
  else
    cli::cli_progress_update(id = scope[["bar"]], inc = 0, status = status_text(msg, .envir))

  invisible(NULL)
}
