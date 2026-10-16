test_that("status_scope() draws and clears a progress display in dynamic terminals", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.width = 120)

  out = capture_status_output(with_progress(fake_loop(c("a", "bad", "skip", "b"))))
  frames = display_frames(out, "Creating repos")
  expect_length(frames, 4)
  expect_match(frames[1], " 1/4 | Created repo \"a\".", fixed = TRUE)
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

  out = capture_status_output(
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

  out = capture_status_output(with_progress(outer()))
  expect_equal(
    visible_lines(out),
    c("v Created 2 of 2 repos", "v Created 2 of 2 repos", "v Outer 2 of 2")
  )
  expect_length(status_env[["scopes"]], 0)
})

test_that("the display is not drawn before cli.progress_show_after and is silenced by suppressMessages()", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE, cli.progress_show_after = 60)

  out = capture_status_output(with_progress(fake_loop(c("a", "bad"))))
  expect_length(display_frames(out, "Creating repos"), 0)
  expect_equal(visible_lines(out)[1], "x Failed to create repo \"bad\".")

  withr::local_options(cli.progress_show_after = 0)
  out = capture_status_output(with_progress(fake_loop(c("a", "b"))))
  expect_length(display_frames(out, "Creating repos"), 2)
  expect_silent(suppressMessages(with_progress(fake_loop(c("a", "b")))))
})

test_that("narrow ANSI terminals put the message on a second line", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = TRUE, cli.width = 60)

  out = capture_status_output(with_progress(fake_loop(c("a", "bad", "b"))))
  frames = display_frames(out, "Creating repos")
  expect_length(frames, 3)
  expect_true(startsWith(frames[1], "\033[?25l\r"))
  expect_match(frames[1], "\n  Created repo \"a\".", fixed = TRUE)
  expect_match(frames[2], "\n  Failed to create repo \"bad\".\033[K\033[1A\r", fixed = TRUE)

  expect_match(out, "\r\033[K\n\033[K\033[1A\r\033[?25hx Failed", fixed = TRUE)
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
  out = capture_status_output(with_progress(fake_loop(c("a", "b"))))
  frames = display_frames(out, "Creating repos")
  expect_match(frames[1], " 1/2 | Created repo \"a\".\033[K\r", fixed = TRUE)
  expect_false(any(grepl("\n", frames, fixed = TRUE)))
  expect_equal(visible_lines(out), "v Created 2 of 2 repos")
})

test_that("suppressed nested operations preserve the visible display for cleanup", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.width = 60)

  for (ansi in c(FALSE, TRUE)) {
    withr::local_options(cli.ansi = ansi)
    out = capture_status_output(with_progress(status_scope("Outer", 2, {
      status_note("Outer status remains on screen.")
      before = as.list(status_env[["display"]])
      suppressMessages(status_scope("Inner", 2, {
        status_note("Hidden inner status.")
      }))
      expect_equal(as.list(status_env[["display"]]), before)
    })))
    expect_equal(visible_lines(out), "i 0 of 2 succeeded")
    expect_false(grepl("Hidden inner", out, fixed = TRUE))
    if (ansi)
      expect_match(out, "\033[?25h", fixed = TRUE)
    expect_equal(status_env[["display"]][["n"]], 0L)
  }
})

test_that("suppressed draws do not change display state or throttle visible draws", {
  local_status_output()
  withr::local_options(cli.dynamic = TRUE, cli.ansi = FALSE)
  local_mocked_bindings(status_now = function() 10)

  out = capture_status_output(with_progress(status_scope("Fetching", 2, {
    before = as.list(status_env[["display"]])
    expect_false(suppressMessages(status_redraw()))
    expect_equal(as.list(status_env[["display"]]), before)
    expect_true(status_redraw())
    expect_false(status_redraw())
  })))
  expect_length(display_frames(out, "Fetching"), 1)
  expect_equal(visible_lines(out), "i 0 of 2 succeeded")
})

test_that("terminal replay preserves ordinary text ending in A", {
  expect_equal(visible_lines("A\n"), "A")
  expect_equal(visible_lines("A\nB\033[1A\rC\n"), c("C", "B"))
})

test_that("redrawing from two lines to one erases the second line", {
  local_status_output()
  withr::local_options(cli.ansi = TRUE)
  withr::defer(capture_status_output(status_display_clear()))

  out = capture_status_output({
    status_display_draw(c("Long progress line", "Previous message"))
    status_display_draw("Short line")
  })
  expect_equal(visible_lines(out), "Short line")
  expect_equal(status_env[["display"]][["n"]], 1L)
})

