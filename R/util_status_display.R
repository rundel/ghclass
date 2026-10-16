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

  if (!status_display_draw(status_lines(scope, now)))
    return(invisible(FALSE))
  display[["last"]] = now

  invisible(TRUE)
}

status_lines = function(scope, now, width = cli::console_width(), ansi = cli::is_ansi_tty()) {
  # The last column stays free so terminals that wrap eagerly do not add a line
  width = max(1L, width - 1L)
  spinner = cli::get_spinner()
  frames = spinner[["frames"]]
  frame = frames[floor(now * 1000 / spinner[["interval"]]) %% length(frames) + 1]

  n_done = scope[["n_ok"]] + scope[["n_fail"]] + scope[["n_skip"]]
  total = scope[["total"]]
  counter = paste0(n_done, "/", total)
  # Keep the counter visible before allocating space to the label and bar.
  if (nchar(counter) >= width)
    return(cli::ansi_strtrim(counter, width))
  head = cli::ansi_strtrim(paste(frame, scope[["name"]]), width - nchar(counter) - 1L)
  status = scope[["status"]]
  has_status = !is.null(status) && nzchar(status)
  message_width = if (has_status) cli::ansi_nchar(status, type = "width") else 0L
  min_bar = 10L
  inline_bar = 20L
  full_bar = 40L
  separator = " | "
  message_space = if (has_status) nchar(separator) + min(40L, message_width) else 0L
  avail = width - cli::ansi_nchar(head, type = "width") - nchar(counter) - 2L

  two_lines = ansi && has_status && avail < inline_bar + message_space
  bar_limit = if (ansi && (two_lines || !has_status)) full_bar else inline_bar
  reserved = if (two_lines) 0L else message_space
  bar_width = max(0L, min(bar_limit, avail, max(min_bar, avail - reserved)))
  parts = c(head, if (bar_width > 0L) status_bar(n_done / total, bar_width), counter)
  lines = paste(parts[nzchar(parts)], collapse = " ")

  if (two_lines) {
    lines = c(lines, cli::ansi_strtrim(paste0("  ", status), width))
  } else if (has_status) {
    status_width = width - cli::ansi_nchar(lines, type = "width") - nchar(separator)
    if (status_width >= min(5L, message_width))
      lines = paste0(lines, separator, cli::ansi_strtrim(status, status_width))
  }

  lines
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
  }

  if (!status_emit(out))
    return(invisible(FALSE))

  display[["n"]] = length(lines)
  display[["width"]] = if (ansi) 0L else w

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

  if (!status_emit(out))
    return(invisible(FALSE))

  display[["n"]] = 0L
  display[["width"]] = 0L
  display[["last"]] = -Inf

  invisible(TRUE)
}

status_cursor = function(action) {
  if (Sys.getenv("R_CLI_HIDE_CURSOR") == "false" || !isTRUE(getOption("cli.hide_cursor", TRUE)))
    return("")

  if (action == "hide") "\033[?25l" else "\033[?25h"
}

# Frames are signaled as messages so suppressMessages() silences them, and
# written to the connection cli uses so they interleave with its output.
# Return whether the frame was written; muffled frames do not change the display.
status_emit = function(txt) {
  cond = structure(
    class = c("ghclass_status_frame", "message", "condition"),
    list(message = txt, call = NULL)
  )

  written = withRestarts(
    muffleMessage = function() FALSE,
    {
      signalCondition(cond)
      cat(txt, file = cli::cli_output_connection(), sep = "")
      TRUE
    }
  )

  invisible(written)
}
