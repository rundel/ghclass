pages_conflict_result = function() {
  e = structure(
    class = c("github_error", "http_error_409", "error", "condition"),
    list(
      message = "GitHub API error (409): Conflict",
      response_content = list(message = "GitHub Pages is already enabled.")
    )
  )
  list(result = NULL, error = e)
}

test_that("pages_create() skips an existing site with the requested settings", {
  local_status_output()
  site = list(build_type = "legacy", source = list(branch = "gh-pages", path = "/"))
  local_mocked_bindings(
    github_api_pages_create = function(...) stop(pages_conflict_result()[["error"]]),
    github_api_pages = function(repo) site
  )

  out = cli::cli_fmt(res <- pages_create("org/a", branch = "gh-pages"))
  expect_equal(out, "i Skipping Pages site for repo \"org/a\", it already exists.")
  expect_equal(res, list(list(result = site, error = NULL)))

  out = cli::cli_fmt(with_progress(pages_create(c("org/a", "org/b"), branch = "gh-pages")))
  expect_equal(out[length(out)], "i Created Pages sites for 0 of 2 repos, 2 skipped")
})

test_that("pages_create() fails when an existing site has different settings", {
  local_status_output()
  local_mocked_bindings(
    github_api_pages_create = function(...) stop(pages_conflict_result()[["error"]]),
    github_api_pages = function(repo) {
      list(build_type = "legacy", source = list(branch = "main", path = "/docs"))
    }
  )

  out = cli::cli_fmt(res <- pages_create("org/a", branch = "gh-pages"))
  expect_equal(out[1], "x Failed to create Pages site for repo \"org/a\".")
  expect_true(failed(res[[1]]))

  out = cli::cli_fmt(res <- pages_create("org/a", build_type = "workflow"))
  expect_equal(out[1], "x Failed to create Pages site for repo \"org/a\".")
})

test_that("pages_create() ignores the source of an existing workflow site", {
  local_status_output()
  local_mocked_bindings(
    github_api_pages_create = function(...) stop(pages_conflict_result()[["error"]]),
    github_api_pages = function(repo) list(build_type = "workflow", source = NULL)
  )

  out = cli::cli_fmt(pages_create("org/a", build_type = "workflow"))
  expect_equal(out, "i Skipping Pages site for repo \"org/a\", it already exists.")
})
