local_status_output = function(.local_envir = parent.frame()) {
  withr::local_options(
    list(
      cli.num_colors = 1, cli.unicode = FALSE, cli.dynamic = FALSE,
      cli.progress_show_after = 0, ghclass.progress = FALSE
    ),
    .local_envir = .local_envir
  )
}

ok_result = function(value = "ok") {
  list(result = value, error = NULL)
}

api_error_result = function() {
  e = simpleError("GitHub API error (422): Unprocessable Entity")
  e[["response_content"]] = list(
    message = "Validation Failed",
    documentation_url = "https://docs.github.com/rest"
  )
  list(result = NULL, error = e)
}

rate_limit_result = function(status = 403) {
  content = switch(
    as.character(status),
    "403" = list(
      message = "You have exceeded a secondary rate limit and have been temporarily blocked from content creation. Please retry your request again later.",
      documentation_url = "/rest/overview/rate-limits-for-the-rest-api#about-secondary-rate-limits"
    ),
    "422" = list(
      message = "Could not clone: was submitted too quickly",
      errors = list("Could not clone: was submitted too quickly"),
      documentation_url = "https://docs.github.com/rest/repos/repos#create-a-repository-using-a-template"
    ),
    "429" = list(message = "Too Many Requests")
  )
  e = structure(
    class = c("github_error", paste0("http_error_", status), "error", "condition"),
    list(message = paste0("GitHub API error (", status, ")"), response_content = content)
  )
  list(result = NULL, error = e)
}

fake_loop = function(items, die_at = NULL, interrupt_at = NULL) {
  status_scope(
    "Creating repos", length(items),
    done = "Created {n_ok} of {total} repo{?s}",
    {
      for (i in items) {
        if (identical(i, die_at))
          stop("unexpected death")
        if (identical(i, interrupt_at))
          rlang::interrupt()

        if (i == "skip") {
          status_skip("Skipping repo {.val {i}}, it already exists.")
        } else if (i == "missing") {
          status_fail("Team {.val {i}} does not exist.")
        } else {
          res = switch(i, bad = api_error_result(), limited = rate_limit_result(), ok_result())
          status_msg(res, "Created repo {.val {i}}.", "Failed to create repo {.val {i}}.")
        }
      }
      items
    }
  )
}

outside_reporter = function() {
  status_msg(ok_result(), "Reporter ran.", "Reporter failed.")
}

# Capture bytes written to the terminal without muffling message conditions.
capture_status_output = function(code) {
  con = rawConnection(raw(), "w")
  previous = getConnection(sink.number(type = "message"))
  on.exit({
    sink(previous, type = "message")
    close(con)
  })
  sink(con, type = "message")
  force(code)
  rawToChar(rawConnectionValue(con))
}

# Replays carriage returns, newlines, erase-to-end-of-line and cursor-up so a
# captured stream shows what a terminal would display at the end
visible_lines = function(out) {
  txt = paste(out, collapse = "")
  tokens = regmatches(
    txt, gregexpr("\033\\[[0-9]*A|\033\\[K|\033\\[\\?25[lh]|\r|\n|[^\033\r\n]+", txt)
  )[[1]]

  rows = ""
  r = 1L
  col = 1L
  for (t in tokens) {
    if (t == "\r") {
      col = 1L
    } else if (t == "\n") {
      r = r + 1L
      col = 1L
      if (r > length(rows)) rows[r] = ""
    } else if (t == "\033[K") {
      rows[r] = substr(rows[r], 1L, col - 1L)
    } else if (grepl("^\033\\[[0-9]*A$", t)) {
      up = as.integer(sub("\033\\[([0-9]*)A", "\\1", t))
      r = max(1L, r - if (is.na(up)) 1L else up)
    } else if (!startsWith(t, "\033[?25")) {
      row = rows[r]
      if (nchar(row) < col - 1L)
        row = paste0(row, strrep(" ", col - 1L - nchar(row)))
      rows[r] = paste0(substr(row, 1L, col - 1L), t, substr(row, col + nchar(t), nchar(row)))
      col = col + nchar(t)
    }
  }

  rows = sub(" +$", "", rows)
  rows[rows != ""]
}

# Progress frames in the captured terminal stream (each starts and ends with CR).
display_frames = function(out, name) {
  frames = regmatches(out, gregexpr("(\033\\[\\?25l)?\r[^\r]*\r", out))[[1]]
  frames[grepl(name, frames, fixed = TRUE)]
}
