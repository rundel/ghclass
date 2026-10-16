#' @name with_progress
#' @rdname with_progress
#'
#' @title Progress mode for status messages
#'
#' @description
#' By default ghclass functions that act on many repositories, teams or users
#' print one line per item. Progress mode replaces the per-item success
#' messages with a progress bar that shows the most recently completed step
#' and prints a single summary line when the operation finishes. Failures and
#' skipped items are still reported in full as they happen. Operations on a single item print
#' their usual message.
#'
#' Progress mode is enabled by setting `options(ghclass.progress = TRUE)` or
#' by wrapping code in `with_progress()`. Functions that do not yet support
#' progress mode print their usual per-item messages.
#'
#' The progress bar is only drawn in dynamic terminals, after the delay set by
#' `options(cli.progress_show_after)` (2 seconds by default). Its layout follows
#' `cli::console_width()`, which can be overridden with `options(cli.width)`:
#' when the console is too narrow for the bar and the latest message to share
#' a line, the message is shown on a second line in terminals that support
#' cursor movement, and is truncated otherwise. The spinner keeps turning while
#' GitHub API requests are waiting on a response, but not during local git
#' operations.
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
  if (!progress_enabled() || total <= 1)
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

  # Static output cannot clear progress lines; keep only failures, skips and summaries.
  scope[["display"]] = cli::is_dynamic_tty()
  scope[["status"]] = NULL
  scope[["show_at"]] = status_now() + getOption("cli.progress_show_after", 2)

  status_env[["scopes"]][[scope[["depth"]]]] = scope
  on.exit(status_scope_pop(scope), add = TRUE)

  res = withCallingHandlers(
    expr,
    message = function(m) {
      if (!inherits(m, "ghclass_status_frame"))
        status_display_clear()
    },
    error = function(e) scope[["state"]] = "error",
    interrupt = function(e) scope[["state"]] = "interrupt"
  )
  scope[["state"]] = "completed"

  res
}

status_scope_pop = function(scope) {
  status_env[["scopes"]] = status_env[["scopes"]][seq_len(scope[["depth"]] - 1L)]
  status_display_clear()
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

  if (!is.null(msg))
    scope[["status"]] = msg
  status_redraw(force = TRUE)

  invisible(TRUE)
}

status_text = function(msg, .envir) {
  cli::format_inline(msg, .envir = .envir, collapse = TRUE)
}

# Reports an item that was skipped before any API call
status_skip = function(msg, n = 1L, .envir = parent.frame()) {
  scope = status_scope_for(.envir)

  cli::cli_alert_info(msg, wrap = FALSE, .envir = .envir)
  if (!is.null(scope))
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

  if (is.null(scope)) {
    cli::cli_alert_success(msg, wrap = FALSE, .envir = .envir)
  } else {
    scope[["status"]] = status_text(msg, .envir)
    status_redraw(force = TRUE)
  }

  invisible(NULL)
}


# The display is the innermost scope's progress line(s) drawn in a dynamic
# terminal. cli progress bars only redraw when the loop ticks and cannot span
# two lines, so the drawing is done here and curl's progress callback redraws
# while requests wait on GitHub.
status_env[["display"]] = list2env(
  list(n = 0L, width = 0L, last = -Inf),
  parent = emptyenv()
)

status_now = function() {
  proc.time()[["elapsed"]]
}

status_display_scope = function() {
  scopes = status_env[["scopes"]]
  n = length(scopes)

  if (n == 0L || !isTRUE(scopes[[n]][["display"]]))
    return(NULL)

  scopes[[n]]
}

status_redraw = function(force = FALSE) {
  scope = status_display_scope()
  if (is.null(scope))
    return(invisible(FALSE))

  display = status_env[["display"]]
  now = status_now()
  interval = cli::get_spinner()[["interval"]] / 1000

  if (now < scope[["show_at"]] || (!force && now - display[["last"]] < interval))
    return(invisible(FALSE))

  display[["last"]] = now
  status_display_draw(status_lines(scope, now))

  invisible(TRUE)
}

status_lines = function(scope, now, width = cli::console_width(), ansi = cli::is_ansi_tty()) {
  # The last column stays free so terminals that wrap eagerly do not add a line
  width = width - 1L
  spinner = cli::get_spinner()
  frames = spinner[["frames"]]
  frame = frames[floor(now * 1000 / spinner[["interval"]]) %% length(frames) + 1]

  n_done = scope[["n_ok"]] + scope[["n_fail"]] + scope[["n_skip"]]
  total = scope[["total"]]
  head = paste(frame, scope[["name"]])
  counter = paste0(n_done, "/", total)
  status = scope[["status"]]
  has_status = !is.null(status) && nzchar(status)

  avail = width - cli::ansi_nchar(head, type = "width") - nchar(counter) - 2L

  # One line needs a 20 column bar, the separator and 40 columns of message
  if (ansi && avail < 63L) {
    bar = status_bar(n_done / total, min(40L, max(10L, avail)))
    lines = cli::ansi_strtrim(paste(head, bar, counter), width)
    if (has_status)
      lines = c(lines, cli::ansi_strtrim(paste0("  ", status), width))
    return(lines)
  }

  bar = status_bar(n_done / total, min(20L, max(10L, avail - 43L)))
  line = paste(head, bar, counter)
  status_width = width - cli::ansi_nchar(line, type = "width") - 3L
  if (has_status && status_width >= 5L)
    line = paste(line, "|", cli::ansi_strtrim(status, status_width))

  cli::ansi_strtrim(line, width)
}

status_bar = function(frac, width) {
  chars = if (cli::is_utf8_output()) c("\u25a0", " ") else c("=", "-")
  n = round(width * min(max(frac, 0), 1))

  cli::col_green(paste0(strrep(chars[1], n), strrep(chars[2], width - n)))
}

# Lines are drawn with the cursor left at the start of the first one, so a
# redraw or clear overwrites in place and foreign output starts there.
status_display_draw = function(lines) {
  display = status_env[["display"]]
  ansi = cli::is_ansi_tty()
  n_old = display[["n"]]

  if (ansi) {
    out = paste0(lines, "\033[K", collapse = "\n")
    extra = n_old - length(lines)
    if (extra > 0L)
      out = paste0(out, strrep("\n\033[K", extra), "\033[", extra, "A")
    if (length(lines) > 1L)
      out = paste0(out, "\033[", length(lines) - 1L, "A")
    # Hidden on every draw since other cli output may have shown the cursor again
    out = paste0(status_cursor("hide"), "\r", out, "\r")
  } else {
    lines = lines[1]
    w = cli::ansi_nchar(lines, type = "width")
    out = paste0("\r", lines, strrep(" ", max(display[["width"]] - w, 0L)), "\r")
    display[["width"]] = w
  }

  display[["n"]] = length(lines)
  status_emit(out)

  invisible(TRUE)
}

status_display_clear = function() {
  display = status_env[["display"]]
  n = display[["n"]]
  if (n == 0L)
    return(invisible(FALSE))

  if (cli::is_ansi_tty()) {
    out = paste0("\r\033[K", strrep("\n\033[K", n - 1L))
    if (n > 1L)
      out = paste0(out, "\033[", n - 1L, "A")
    out = paste0(out, "\r", status_cursor("show"))
  } else {
    out = paste0("\r", strrep(" ", display[["width"]]), "\r")
  }

  display[["n"]] = 0L
  display[["width"]] = 0L
  display[["last"]] = -Inf
  status_emit(out)

  invisible(TRUE)
}

status_cursor = function(action) {
  if (Sys.getenv("R_CLI_HIDE_CURSOR") == "false" || !isTRUE(getOption("cli.hide_cursor", TRUE)))
    return("")

  if (action == "hide") "\033[?25l" else "\033[?25h"
}

# Frames are signaled as messages so suppressMessages() silences them, and
# written to the connection cli uses so they interleave with its output.
status_emit = function(txt) {
  cond = structure(
    class = c("ghclass_status_frame", "message", "condition"),
    list(message = txt, call = NULL)
  )

  withRestarts(
    muffleMessage = function() NULL,
    {
      signalCondition(cond)
      cat(txt, file = cli::cli_output_connection(), sep = "")
    }
  )

  invisible(NULL)
}

# curl calls this roughly once a second while a request is idle and more
# often while data moves; it must never error and must return a logical.
status_curl_callback = function(down, up) {
  tryCatch(status_redraw(), error = function(e) NULL)
  TRUE
}

# httr2 has no global hook for curl options, so requests made while a
# display is active go through its mock hook and are performed with the
# progress callback attached.
status_httr2_hook = function() {
  if (is.null(status_display_scope()))
    return(NULL)

  function(req) {
    withr::local_options(list(httr2_mock = NULL))
    req = httr2::req_options(req, noprogress = FALSE, xferinfofunction = status_curl_callback)
    httr2::req_perform(req)
  }
}

status_httr_config = function() {
  if (is.null(status_display_scope()))
    return(httr::config())

  httr::config(noprogress = FALSE, progressfunction = status_curl_callback)
}
