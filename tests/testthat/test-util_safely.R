test_that("error_msg_tree() handles errors without details", {
  res = purrr::safely(stop)("plain failure")

  tree = error_msg_tree(error_msg(res))

  expect_length(tree, 1)
  expect_match(tree, "plain failure")
})

test_that("status_msg() reports failures as cli messages", {
  withr::local_options(cli.num_colors = 1, cli.unicode = FALSE)

  e = simpleError("GitHub API error (422): Validation {failed} {.val x}")
  e[["response_content"]] = list(
    message = "Repository creation failed.",
    errors = list(list(message = "name already exists on this account")),
    documentation_url = "https://docs.github.com/rest/repos#create"
  )
  res = list(result = NULL, error = e)

  out = cli::cli_fmt(status_msg(res, "ok", "Failed to create repo {.val a/b}."))
  expect_equal(
    out,
    c(
      "x Failed to create repo \"a/b\".",
      "\\-GitHub API error (422): Validation {failed} {.val x}",
      "  +- API message: Repository creation failed.; name already exists on this account",
      "  \\- API docs: https://docs.github.com/rest/repos#create"
    )
  )

  expect_silent(suppressMessages(status_msg(res, "ok", "Failed to create repo {.val a/b}.")))
  expect_silent(suppressMessages(status_msg(list(result = "ok", error = NULL), "Created {.val a/b}.", "fail")))
})
