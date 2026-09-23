local_status_output = function(.local_envir = parent.frame()) {
  withr::local_options(
    list(
      cli.num_colors = 1, cli.unicode = FALSE, cli.dynamic = FALSE,
      cli.progress_show_after = 0
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
          res = if (i == "bad") api_error_result() else ok_result()
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
