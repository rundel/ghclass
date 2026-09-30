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

test_that("is_rate_limit_error() recognizes secondary rate limit responses", {
  expect_true(is_rate_limit_error(error(rate_limit_result(403))))
  expect_true(is_rate_limit_error(error(rate_limit_result(422))))
  expect_true(is_rate_limit_error(error(rate_limit_result(429))))

  expect_false(is_rate_limit_error(NULL))
  expect_false(is_rate_limit_error(error(api_error_result())))
  expect_false(is_rate_limit_error(simpleError("plain failure")))

  forbidden = structure(
    class = c("github_error", "http_error_403", "error", "condition"),
    list(message = "GitHub API error (403)", response_content = list(message = "Resource not accessible by personal access token"))
  )
  expect_false(is_rate_limit_error(forbidden))
})

test_that("status_msg() aborts on a secondary rate limit after reporting the failure", {
  withr::local_options(cli.num_colors = 1, cli.unicode = FALSE)

  out = cli::cli_fmt(
    err <- tryCatch(status_msg(rate_limit_result(), "ok", "Failed to create repo {.val a/b}."), error = identity)
  )
  expect_s3_class(err, "ghclass_rate_limit_error")
  expect_match(conditionMessage(err), "temporarily blocked content creation")
  expect_equal(out[1], "x Failed to create repo \"a/b\".")
  expect_match(out[3], "secondary rate limit")

  expect_silent(suppressMessages(status_msg(api_error_result(), "ok", "fail")))
})
