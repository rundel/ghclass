test_that("repo_create() counts unique repos and skips existing ones", {
  local_status_output()
  local_mocked_bindings(
    repo_exists = function(repo, ...) repo == "org/b",
    github_api_org_repo_create = function(repo, ...) list(full_name = repo)
  )

  out = cli::cli_fmt(res <- with_progress(repo_create("org", c("a", "b", "a"))))
  expect_equal(out, "v Created 1 of 2 repos, 1 skipped")
  expect_equal(res, c("org/a", "org/b"))

  out = cli::cli_fmt(repo_create("org", c("a", "b", "a")))
  expect_equal(
    out,
    c("v Created repo \"org/a\".", "i Skipping repo \"org/b\", it already exists.")
  )
})

test_that("team_create() counts grouped skips", {
  local_status_output()
  local_mocked_bindings(
    org_teams = function(org, ...) c("t1", "t2"),
    github_api_team_create = function(org, name, ...) list(name = name)
  )

  out = cli::cli_fmt(with_progress(team_create("org", c("t1", "t2", "t3", "t1"))))
  expect_equal(out, "v Created 1 of 3 teams, 2 skipped")

  out = cli::cli_fmt(team_create("org", c("t1", "t2", "t3", "t1")))
  expect_equal(
    out,
    c(
      "i Skipping existing teams: \"t1\" and \"t2\".",
      "v Created team \"t3\" in org \"org\"."
    )
  )
})

test_that("team_invite() counts missing teams as failures", {
  local_status_output()
  local_mocked_bindings(
    team_slug_lookup = function(org, name) ifelse(name == "missing", NA, name),
    github_api_team_invite = function(org, team_slug, username, ...) list(state = "active")
  )

  out = cli::cli_fmt(
    with_progress(team_invite("org", user = c("u1", "u2"), team = c("t1", "missing")))
  )
  expect_equal(
    out,
    c(
      "x Team \"missing\" does not exist in org \"org\".",
      "x Added 1 of 2 users to teams, 1 failed"
    )
  )
})

test_that("repo_add_team() reports missing teams", {
  local_status_output()
  local_mocked_bindings(
    team_slug_lookup = function(org, name) ifelse(name == "missing", NA, name),
    github_api_team_add = function(org, team_slug, repo, ...) list()
  )

  out = cli::cli_fmt(
    with_progress(repo_add_team(c("org/r1", "org/r2"), c("t1", "missing")))
  )
  expect_equal(
    out,
    c(
      "x Team \"missing\" does not exist in org \"org\".",
      "x Gave 1 of 2 teams \"push\" access to repos, 1 failed"
    )
  )

  out = cli::cli_fmt(repo_add_team(c("org/r1", "org/r2"), c("t1", "missing")))
  expect_equal(
    out,
    c(
      "v Team \"t1\" given \"push\" access to repo \"org/r1\"",
      "x Team \"missing\" does not exist in org \"org\"."
    )
  )
})

test_that("repo_add_file() counts files and keeps API failure details", {
  local_status_output()
  files = withr::local_tempfile(lines = "x", fileext = ".txt")
  files = c(files, withr::local_tempfile(lines = "y", fileext = ".txt"))

  local_mocked_bindings(
    file_exists = function(...) FALSE,
    repo_put_file = function(repo, path, ...) {
      if (path == fs::path_file(files[2])) api_error_result() else ok_result()
    }
  )

  out = cli::cli_fmt(with_progress(repo_add_file("org/r1", files)))
  expect_equal(
    out,
    c(
      paste0("x Failed to add file \"", fs::path_file(files[2]), "\" to repo \"org/r1\"."),
      "\\-GitHub API error (422): Unprocessable Entity",
      "  +- API message: Validation Failed",
      "  \\- API docs: https://docs.github.com/rest",
      "x Added 1 of 2 files, 1 failed"
    )
  )

  out = cli::cli_fmt(with_progress(repo_add_file(c("org/r1", "org/r2"), files[1])))
  expect_equal(out, "v Added 2 of 2 files")
})

test_that("action_add_badge() counts repos after grouping workflows", {
  local_status_output()
  local_mocked_bindings(
    modify_file = function(...) ok_result()
  )

  out = cli::cli_fmt(with_progress(action_add_badge("org/r1", workflow = c("w1", "w2"))))
  expect_equal(out, "v Added badges to 1 of 1 repo")
})

test_that("org_create_assignment() prints one summary per step", {
  local_status_output()
  local_mocked_bindings(
    repo_exists = function(repo, ...) rep(FALSE, length(repo)),
    github_api_org_repo_create = function(repo, ...) list(full_name = repo),
    org_teams = function(org, ...) character(),
    github_api_team_create = function(org, name, ...) list(name = name),
    team_slug_lookup = function(org, name) name,
    github_api_team_invite = function(...) list(state = "active"),
    github_api_team_add = function(...) list()
  )

  out = cli::cli_fmt(
    with_progress(
      org_create_assignment(
        "org", repo = c("hw1-a", "hw1-b"), user = c("a", "b"), team = c("ta", "tb")
      )
    )
  )
  expect_equal(
    out,
    c(
      "v Created 2 of 2 repos",
      "v Created 2 of 2 teams",
      "v Added 2 of 2 users to teams",
      "v Gave 2 of 2 teams \"push\" access to repos"
    )
  )
})
