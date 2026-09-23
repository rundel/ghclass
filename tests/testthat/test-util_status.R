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

test_that("status_scope() draws and clears a progress bar in dynamic terminals", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE)

  out = cli::cli_fmt(with_progress(fake_loop(c("a", "bad", "skip", "b"))))
  expect_match(out[1], "Creating repos", fixed = TRUE)
  expect_match(out[1], "1/4 | Created repo \"a\".", fixed = TRUE)
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

  out = cli::cli_fmt(
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

  out = cli::cli_fmt(with_progress(outer()))
  expect_equal(
    visible_lines(out),
    c("v Created 2 of 2 repos", "v Created 2 of 2 repos", "v Outer 2 of 2")
  )
  expect_length(status_env[["scopes"]], 0)
})
