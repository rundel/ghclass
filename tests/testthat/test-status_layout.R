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

test_that("layout keeps short messages inline and reserves room for the counter", {
  local_status_output()
  withr::local_options(cli.num_colors = 256, cli.unicode = TRUE)
  scope = list2env(list(
    name = cli::col_blue("Creating repos"), total = 20, n_ok = 7L, n_fail = 0L, n_skip = 0L,
    status = "OK"
  ))

  for (ansi in c(FALSE, TRUE)) {
    lines = status_lines(scope, now = 0, width = 60, ansi = ansi)
    expect_length(lines, 1)
    expect_match(lines, "7/20 | OK", fixed = TRUE)

    for (width in c(8, 20, 28, 36, 60, 100)) {
      lines = status_lines(scope, now = 0, width = width, ansi = ansi)
      expect_match(lines[1], "7/20", fixed = TRUE)
      expect_true(all(cli::ansi_nchar(lines, type = "width") < width))
    }
  }
})
