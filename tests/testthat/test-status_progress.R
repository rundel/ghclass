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

test_that("action_runs() counts workflows across repos", {
  local_status_output()
  local_mocked_bindings(
    action_workflows = function(repo, ...) tibble::tibble(name = c("w1", "w2"), id = c(1L, 2L)),
    github_api_action_workflow_runs = function(repo, workflow_id, ...) {
      list(total_count = 1, workflow_runs = list(list(
        id = workflow_id, head_branch = "main", head_sha = "abc", actor = list(login = "a"),
        event = "push", status = "completed", conclusion = "success",
        created_at = "2024-01-01T00:00:00Z"
      )))
    }
  )

  out = cli::cli_fmt(res <- with_progress(action_runs(c("org/r1", "org/r2"))))
  expect_equal(out, "v Retrieved runs for 4 of 4 workflows")
  expect_equal(nrow(res), 4)
  expect_equal(res[["workflow"]], c("w1", "w2", "w1", "w2"))
})

test_that("issue_close() counts each issue once", {
  local_status_output()
  local_mocked_bindings(
    github_api_issue_comment = function(repo, number, body) {
      if (number == 2) stop("GitHub API error (404): Not Found") else list(id = 1)
    },
    github_api_issue_edit = function(repo, number, ...) list(state = "closed")
  )

  out = cli::cli_fmt(with_progress(issue_close("org/r", c(1, 2), comment = "Done")))
  expect_equal(
    out,
    c(
      "x Failed to comment on issue \"#2\" for repo \"org/r\".",
      "\\-GitHub API error (404): Not Found",
      "x Closed 1 of 2 issues, 1 failed"
    )
  )

  out = cli::cli_fmt(issue_close("org/r", 1, comment = "Done"))
  expect_equal(
    out,
    c(
      "v Commented on issue \"#1\" for repo \"org/r\".",
      "v Closed issue \"#1\" for repo \"org/r\"."
    )
  )
})

test_that("branch_create() counts skips and missing branches", {
  local_status_output()
  local_mocked_bindings(
    repo_branches = function(repo, ...) c("main", "dev"),
    github_api_branch_create = function(repo, branch, new_branch) list(ref = new_branch)
  )

  out = cli::cli_fmt(
    with_progress(branch_create("org/r", c("main", "nope", "main"), c("dev", "x", "new")))
  )
  expect_length(out, 2)
  expect_match(out[1], "^x Failed to create branch, .*nope.* does not exist\\.$")
  expect_equal(out[2], "x Created 1 of 3 branches, 1 failed, 1 skipped")
})

test_that("team_delete() and team_members() report missing teams", {
  local_status_output()
  local_mocked_bindings(
    team_slug_lookup = function(org, name) ifelse(name == "missing", NA, name),
    github_api_team_delete = function(org, team_slug) list(),
    github_api_team_members = function(org, team_slug, ...) list(list(login = "u1"))
  )

  out = cli::cli_fmt(with_progress(team_delete("org", c("t1", "missing"), prompt = FALSE)))
  expect_equal(
    out,
    c(
      "x Team \"missing\" does not exist in org \"org\".",
      "x Deleted 1 of 2 teams from org \"org\", 1 failed"
    )
  )

  out = cli::cli_fmt(res <- with_progress(team_members("org", c("t1", "missing"))))
  expect_equal(
    out,
    c(
      "x Team \"missing\" does not exist in org \"org\".",
      "x Retrieved members for 1 of 2 teams, 1 failed"
    )
  )
  expect_equal(res[["user"]], "u1")
})

test_that("verbose and quiet arguments suppress the summary", {
  local_status_output()
  local_mocked_bindings(
    modify_file = function(...) ok_result(),
    github_api_repo_commits = function(repo, ...) {
      if (repo == "org/bad") stop("GitHub API error (404): Not Found") else list()
    }
  )

  out = cli::cli_fmt(
    with_progress(repo_modify_file(c("org/r1", "org/r2"), "README.md", "a", "b", verbose = FALSE))
  )
  expect_equal(out, character())

  out = cli::cli_fmt(
    with_progress(repo_modify_file(c("org/r1", "org/r2"), "README.md", "a", "b"))
  )
  expect_equal(out, "v Modified 2 of 2 files")

  out = cli::cli_fmt(with_progress(repo_commits(c("org/r1", "org/bad"), quiet = TRUE)))
  expect_equal(out, character())

  out = cli::cli_fmt(with_progress(repo_commits(c("org/r1", "org/bad"))))
  expect_equal(
    out,
    c(
      "x Failed to retrieve commits from \"org/bad\".",
      "\\-GitHub API error (404): Not Found",
      "x Retrieved commits for 1 of 2 repos, 1 failed"
    )
  )
})
