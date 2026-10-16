test_that("status helpers are unchanged when progress mode is off", {
  local_status_output()
  withr::local_options(list(ghclass.progress = FALSE))

  expect_equal(
    cli::cli_fmt(fake_loop(c("a", "skip", "bad", "missing"))),
    c(
      "v Created repo \"a\".",
      "i Skipping repo \"skip\", it already exists.",
      "x Failed to create repo \"bad\".",
      "\\-GitHub API error (422): Unprocessable Entity",
      "  +- API message: Validation Failed",
      "  \\- API docs: https://docs.github.com/rest",
      "x Team \"missing\" does not exist."
    )
  )
  expect_length(status_env[["scopes"]], 0)
})

test_that("status_scope() summarizes mixed results and keeps failure details", {
  local_status_output()

  out = cli::cli_fmt(res <- with_progress(fake_loop(c("a", "b", "skip", "bad", "missing"))))
  expect_equal(
    out,
    c(
      "i Skipping repo \"skip\", it already exists.",
      "x Failed to create repo \"bad\".",
      "\\-GitHub API error (422): Unprocessable Entity",
      "  +- API message: Validation Failed",
      "  \\- API docs: https://docs.github.com/rest",
      "x Team \"missing\" does not exist.",
      "x Created 2 of 5 repos, 2 failed, 1 skipped"
    )
  )
  expect_equal(res, c("a", "b", "skip", "bad", "missing"))
  expect_length(status_env[["scopes"]], 0)
})

test_that("status_scope() summarizes success and all skipped, and bypasses single and empty input", {
  local_status_output()

  expect_equal(cli::cli_fmt(with_progress(fake_loop(c("a", "b")))), "v Created 2 of 2 repos")
  expect_equal(cli::cli_fmt(with_progress(fake_loop("a"))), "v Created repo \"a\".")
  expect_equal(
    cli::cli_fmt(with_progress(fake_loop(c("skip", "skip")))),
    c("i Skipping repo \"skip\", it already exists.", "i Skipping repo \"skip\", it already exists.", "i Created 0 of 2 repos, 2 skipped")
  )
  expect_equal(cli::cli_fmt(with_progress(fake_loop(character()))), character())
})

test_that("events belong to the innermost scope only", {
  local_status_output()

  out = cli::cli_fmt(
    with_progress(
      status_scope(
        "Outer", 2, done = "Outer finished {n_ok} of {total}",
        {
          fake_loop(c("a", "b"))
          fake_loop(c("skip", "skip"))
          status_msg(ok_result(), "Outer item done.", "Outer item failed.")
        }
      )
    )
  )
  expect_equal(
    out,
    c(
      "v Created 2 of 2 repos",
      "i Skipping repo \"skip\", it already exists.",
      "i Skipping repo \"skip\", it already exists.",
      "i Created 0 of 2 repos, 2 skipped",
      "v Outer finished 1 of 2"
    )
  )
  expect_length(status_env[["scopes"]], 0)
})

test_that("status_scope() reports aborted work and preserves the error", {
  local_status_output()

  out = cli::cli_fmt(
    err <- tryCatch(with_progress(fake_loop(c("a", "b", "c"), die_at = "c")), error = identity)
  )
  expect_s3_class(err, "simpleError")
  expect_equal(conditionMessage(err), "unexpected death")
  expect_equal(out, "x Creating repos aborted after 2 of 3: 2 succeeded")
  expect_length(status_env[["scopes"]], 0)
  expect_false(isTRUE(getOption("ghclass.progress")))
})

test_that("status_scope() reports aborted work on interrupt", {
  local_status_output()

  out = cli::cli_fmt(
    res <- tryCatch(
      with_progress(fake_loop(c("bad", "skip", "c"), interrupt_at = "c")),
      interrupt = function(e) "interrupted"
    )
  )
  expect_equal(res, "interrupted")
  expect_equal(
    out[length(out)],
    "x Creating repos aborted after 2 of 3: 1 failed, 1 skipped"
  )
  expect_length(status_env[["scopes"]], 0)
  expect_false(isTRUE(getOption("ghclass.progress")))
})

test_that("with_progress() preserves values, visibility, and nests", {
  local_status_output()

  expect_equal(with_progress(1 + 1), 2)
  expect_true(withVisible(with_progress(1 + 1))[["visible"]])
  expect_false(withVisible(with_progress(invisible(1)))[["visible"]])
  expect_true(with_progress(getOption("ghclass.progress")))
  expect_false(isTRUE(getOption("ghclass.progress")))

  out = cli::cli_fmt(with_progress(with_progress(fake_loop(c("a", "b")))))
  expect_equal(out, "v Created 2 of 2 repos")
})

test_that("reporters called from inside a loop are not counted by its scope", {
  local_status_output()

  out = cli::cli_fmt(
    with_progress(
      status_scope(
        "Outer", 2, done = "Outer {n_ok} of {total}",
        {
          outside_reporter()
          status_msg(ok_result(), "Own item.", "Own item failed.")
        }
      )
    )
  )
  expect_equal(out, c("v Reporter ran.", "v Outer 1 of 2"))
})

test_that("status_msg() counts outcomes without messages and status_note() counts nothing", {
  local_status_output()

  out = cli::cli_fmt(
    with_progress(
      status_scope(
        "Fetching", 3, done = "Fetched {n_ok} of {total}",
        {
          status_msg(ok_result(), fail = "Failed one.")
          status_note("Half way there.")
          status_msg(api_error_result(), fail = NULL)
          status_msg(ok_result())
        }
      )
    )
  )
  expect_equal(out, "x Fetched 2 of 3, 1 failed")

  out = cli::cli_fmt({
    status_msg(ok_result(), fail = "Failed one.")
    status_note("Half way there.")
    status_msg(api_error_result(), fail = NULL)
  })
  expect_equal(out, "v Half way there.")
})

test_that("status_scope() draws and clears a progress display in dynamic terminals", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.width = 120)

  out = capture_messages(with_progress(fake_loop(c("a", "bad", "skip", "b"))))
  frames = display_frames(out, "Creating repos")
  expect_length(frames, 4)
  expect_match(frames[1], "^\r[-\\\\|/] Creating repos =* *-* 1/4 \\| Created repo \"a\"\\. *\r$")
  expect_match(frames[4], " 4/4 | Created repo \"b\".", fixed = TRUE)
  expect_false(any(grepl("\n", frames, fixed = TRUE)))
  expect_equal(
    visible_lines(out),
    c(
      "x Failed to create repo \"bad\".",
      "\\-GitHub API error (422): Unprocessable Entity",
      "  +- API message: Validation Failed",
      "  \\- API docs: https://docs.github.com/rest",
      "i Skipping repo \"skip\", it already exists.",
      "x Created 2 of 4 repos, 1 failed, 1 skipped"
    )
  )
  expect_length(status_env[["scopes"]], 0)
  expect_equal(status_env[["display"]][["n"]], 0L)

  out = capture_messages(
    expect_error(with_progress(fake_loop(c("a", "b", "c"), die_at = "b")), "unexpected death")
  )
  expect_equal(visible_lines(out), "x Creating repos aborted after 1 of 3: 1 succeeded")
  expect_length(status_env[["scopes"]], 0)

  outer = function() {
    status_scope(
      "Outer", 2, done = "Outer {n_ok} of {total}",
      purrr::walk(1:2, function(i) {
        fake_loop(c("a", "b"))
        status_msg(ok_result(), "Outer item {i}.")
      })
    )
  }

  out = capture_messages(with_progress(outer()))
  expect_equal(
    visible_lines(out),
    c("v Created 2 of 2 repos", "v Created 2 of 2 repos", "v Outer 2 of 2")
  )
  expect_length(status_env[["scopes"]], 0)
})

test_that("the display is not drawn before cli.progress_show_after and is silenced by suppressMessages()", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.progress_show_after = 60)

  out = capture_messages(with_progress(fake_loop(c("a", "bad"))))
  expect_length(display_frames(out, "Creating repos"), 0)
  expect_equal(visible_lines(out)[1], "x Failed to create repo \"bad\".")

  withr::local_options(cli.progress_show_after = 0)
  out = capture_messages(with_progress(fake_loop(c("a", "b"))))
  expect_length(display_frames(out, "Creating repos"), 2)
  expect_silent(suppressMessages(with_progress(fake_loop(c("a", "b")))))
})

test_that("narrow ANSI terminals put the message on a second line", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE, cli.width = 60)

  out = capture_messages(with_progress(fake_loop(c("a", "bad", "b"))))
  frames = display_frames(out, "Creating repos")
  expect_length(frames, 3)
  expect_match(
    frames[1],
    "^\033\\[\\?25l\r[-\\\\|/] Creating repos =* *-* 1/3\033\\[K\n  Created repo \"a\"\\.\033\\[K\033\\[1A\r$"
  )
  expect_match(frames[2], "\n  Failed to create repo \"bad\".\033[K\033[1A\r", fixed = TRUE)

  cleared = out[which(startsWith(out, "x Failed")) - 1]
  expect_equal(cleared, "\r\033[K\n\033[K\033[1A\r\033[?25h")
  expect_equal(
    visible_lines(out),
    c(
      "x Failed to create repo \"bad\".",
      "\\-GitHub API error (422): Unprocessable Entity",
      "  +- API message: Validation Failed",
      "  \\- API docs: https://docs.github.com/rest",
      "x Created 2 of 3 repos, 1 failed"
    )
  )
  expect_equal(status_env[["display"]][["n"]], 0L)

  withr::local_options(cli.width = 120)
  out = capture_messages(with_progress(fake_loop(c("a", "b"))))
  frames = display_frames(out, "Creating repos")
  expect_match(frames[1], " 1/2 | Created repo \"a\".\033[K\r", fixed = TRUE)
  expect_false(any(grepl("\n", frames, fixed = TRUE)))
  expect_equal(visible_lines(out), "v Created 2 of 2 repos")
})

test_that("status_lines() truncates and drops the message as the console narrows", {
  local_status_output()
  withr::local_options(cli.width = 80)
  scope = list2env(list(
    name = "Creating repos", total = 20, n_ok = 7L, n_fail = 0L, n_skip = 0L,
    status = strrep("long message ", 10)
  ))

  wide = status_lines(scope, now = 0, width = 100, ansi = TRUE)
  expect_length(wide, 1)
  expect_match(wide, "^[-\\\\|/] Creating repos =+-+ 7/20 \\| long message .*\\.\\.\\.$")
  expect_lte(nchar(wide), 100)

  narrow = status_lines(scope, now = 0, width = 60, ansi = TRUE)
  expect_length(narrow, 2)
  expect_match(narrow[1], "^[-\\\\|/] Creating repos =+-+ 7/20$")
  expect_match(narrow[2], "^  long message .*\\.\\.\\.$")
  expect_lte(max(nchar(narrow)), 60)

  rstudio = status_lines(scope, now = 0, width = 60, ansi = FALSE)
  expect_length(rstudio, 1)
  expect_match(rstudio, "^[-\\\\|/] Creating repos =+-+ 7/20 \\| long .*\\.\\.\\.$")
  expect_lte(nchar(rstudio), 60)

  tiny = status_lines(scope, now = 0, width = 36, ansi = FALSE)
  expect_match(tiny, "^[-\\\\|/] Creating repos =+-+ 7/20$")

  scope[["status"]] = NULL
  expect_length(status_lines(scope, now = 0, width = 60, ansi = TRUE), 1)
})

test_that("API requests redraw the display while they wait", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.progress_show_after = 0)
  expect_null(status_httr2_hook())
  expect_null(status_httr_config()[["options"]][["noprogress"]])
  expect_true(status_curl_callback(c(0, 0), c(0, 0)))

  seen = NULL
  local_mocked_bindings(
    req_perform = function(req, ...) {
      seen <<- req[["options"]]
      "resp"
    },
    .package = "httr2"
  )

  out = capture_messages(
    with_progress(
      status_scope("Fetching", 2, {
        hook = status_httr2_hook()
        expect_type(hook, "closure")
        expect_equal(hook(httr2::request("https://example.com")), "resp")
        expect_false(seen[["noprogress"]])
        expect_true(seen[["xferinfofunction"]](c(0, 0), c(0, 0)))

        config = status_httr_config()
        expect_false(config[["options"]][["noprogress"]])
        expect_identical(config[["options"]][["progressfunction"]], status_curl_callback)

        status_msg(ok_result(), "Fetched one.")
        status_msg(ok_result(), "Fetched two.")
      })
    )
  )
  frames = display_frames(out, "Fetching")
  expect_match(frames[1], " 0/2 *\r$")
  expect_match(frames[2], " 1/2 | Fetched one.", fixed = TRUE)
  expect_equal(visible_lines(out), "v 2 of 2 succeeded")

  local_mocked_bindings(status_redraw = function(...) stop("boom"))
  expect_true(status_curl_callback(c(0, 0), c(0, 0)))
})

test_that("ghclass_api_v3_req() attaches the hook except for downloads", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.progress_show_after = 0)
  local_mocked_bindings(github_get_token = function(...) "token")
  local_mocked_bindings(
    gh = function(...) {
      message("gh noise")
      status_redraw(force = TRUE)
      list(mock = getOption("httr2_mock"))
    },
    .package = "gh"
  )

  expect_null(ghclass_api_v3_req("GET /user")[["mock"]])

  out = capture_messages(
    with_progress(
      status_scope("Fetching", 2, {
        expect_type(ghclass_api_v3_req("GET /user")[["mock"]], "closure")
        expect_null(ghclass_api_v3_req("GET /user", .destfile = tempfile())[["mock"]])
      })
    )
  )
  expect_false(any(grepl("gh noise", out, fixed = TRUE)))
  expect_length(display_frames(out, "Fetching"), 2)
})

test_that("status_scope() aborts the batch on a secondary rate limit", {
  local_status_output()

  out = cli::cli_fmt(
    err <- tryCatch(with_progress(fake_loop(c("a", "limited", "c"))), error = identity)
  )
  expect_s3_class(err, "ghclass_rate_limit_error")
  expect_equal(out[1], "x Failed to create repo \"limited\".")
  expect_equal(out[length(out)], "x Creating repos aborted after 2 of 3: 1 succeeded, 1 failed")
  expect_length(status_env[["scopes"]], 0)

  withr::local_options(list(ghclass.progress = FALSE))
  out = cli::cli_fmt(
    err <- tryCatch(fake_loop(c("a", "limited", "c")), error = identity)
  )
  expect_s3_class(err, "ghclass_rate_limit_error")
  expect_equal(out[1:2], c("v Created repo \"a\".", "x Failed to create repo \"limited\"."))
  expect_false(any(grepl("\"c\"", out)))
})
