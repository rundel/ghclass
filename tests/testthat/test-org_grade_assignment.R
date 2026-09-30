test_that("flag_stale_artifacts() flags artifacts built from a commit other than the repo head", {
  ids = tibble::tibble(
    repo = c("org/hw1-a", "org/hw1-b", "org/hw1-c", "org/hw1-d", "org/hw1-e"),
    name = "html",
    id = 1:5,
    commit = c("aaa", "bbb", NA, "ddd", "eee")
  )
  head_sha = c("org/hw1-a" = "aaa", "org/hw1-b" = "zzz", "org/hw1-c" = "ccc", "org/hw1-d" = NA)

  res = flag_stale_artifacts(ids, head_sha)

  expect_equal(res[["repo_commit"]], c("aaa", "zzz", "ccc", NA, NA))
  expect_equal(res[["stale"]], c(FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that("flag_stale_artifacts() handles empty inputs", {
  ids = tibble::tibble(repo = character(), name = character(), id = double(), commit = character())

  res = flag_stale_artifacts(ids, c("org/hw1-a" = "aaa"))

  expect_equal(nrow(res), 0)
  expect_equal(res[["stale"]], logical())
})

test_that("local_repo_head_sha() returns the head commit or NA", {
  skip_if_not_installed("gert")

  dir = withr::local_tempdir()
  repo = fs::path(dir, "hw1-a")
  fs::dir_create(repo)
  writeLines("hi", fs::path(repo, "README.md"))
  gert::git_init(repo)
  gert::git_add("README.md", repo = repo)
  gert::git_commit("init", repo = repo, author = gert::git_signature("A", "a@b.c"))

  not_repo = fs::path(dir, "hw1-b")
  fs::dir_create(not_repo)

  dirs = c("org/hw1-a" = as.character(repo), "org/hw1-b" = as.character(not_repo), "org/hw1-c" = NA)
  res = local_repo_head_sha(dirs)

  expect_named(res, names(dirs))
  expect_equal(unname(res[1]), gert::git_commit_id(repo = repo))
  expect_true(all(is.na(res[2:3])))
})

test_that("report_stale_artifacts() describes skipped and allowed artifacts", {
  stale = tibble::tibble(
    repo = "org/hw1-b", name = "html", commit = "bbbbbbbbbb", repo_commit = "zzzzzzzzzz"
  )

  skipped = paste(capture_messages(report_stale_artifacts("html", stale, allow_stale = FALSE)), collapse = "")
  expect_match(skipped, "skipping 1 artifact")
  expect_match(skipped, "built from bbbbbbb, repo is at zzzzzzz")

  allowed = paste(capture_messages(report_stale_artifacts("html", stale, allow_stale = TRUE)), collapse = "")
  expect_match(allowed, "downloading it anyway")
})

local_grade_assignment_mocks = function(.env = parent.frame()) {
  local_mocked_bindings(
    repo_exists = function(repo, ...) TRUE,
    org_repos = function(org, filter, ...) c("org/hw1-a", "org/hw1-b"),
    local_repo_clone = function(repo, local_path = ".", ...) {
      dirs = file.path(local_path, get_repo_name(repo))
      purrr::walk(dirs, dir.create, recursive = TRUE)
      stats::setNames(dirs, repo)
    },
    .env = .env
  )
}

test_that("org_grade_assignment() refuses to overwrite existing grading folders", {
  local_grade_assignment_mocks()
  local_mocked_bindings(
    org_repos = function(...) stop("should not be reached")
  )

  path = withr::local_tempdir()
  dir.create(file.path(path, "comments"))
  writeLines("graded", file.path(path, "comments", "hw1-a.md"))
  dir.create(file.path(path, "hw1-key"))

  expect_error(
    org_grade_assignment(path, "org", "hw1-", artifacts = c("html" = "html-output"), key_repo = "org/hw1-key"),
    "already contains .*comments.* and .*hw1-key.*overwrite = TRUE.* to replace them"
  )
  expect_equal(readLines(file.path(path, "comments", "hw1-a.md")), "graded")

  dir.create(file.path(path, "other", "html"), recursive = TRUE)
  expect_error(
    org_grade_assignment(file.path(path, "other"), "org", "hw1-", artifacts = c("html" = "html-output")),
    "already contains .*html.* to replace it"
  )
})

test_that("org_grade_assignment() uses an existing folder without conflicts", {
  local_grade_assignment_mocks()

  path = withr::local_tempdir()
  writeLines("notes", file.path(path, "notes.md"))

  suppressMessages({
    res = org_grade_assignment(path, "org", "hw1-", comment_template = "template")
  })

  expect_equal(res[["comments"]], file.path(path, "comments", c("hw1-a.md", "hw1-b.md")))
  expect_true(all(dir.exists(file.path(path, "repos", c("hw1-a", "hw1-b")))))
  expect_equal(readLines(file.path(path, "notes.md")), "notes")
})

test_that("org_grade_assignment() replaces only its own folders when overwrite = TRUE", {
  local_grade_assignment_mocks()

  path = withr::local_tempdir()
  writeLines("notes", file.path(path, "notes.md"))
  dir.create(file.path(path, "repos", "hw1-old"), recursive = TRUE)
  dir.create(file.path(path, "comments"))
  writeLines("graded", file.path(path, "comments", "hw1-a.md"))
  dir.create(file.path(path, "hw1-key"))
  writeLines("old", file.path(path, "hw1-key", "key.md"))

  msgs = capture_messages({
    res = org_grade_assignment(
      path, "org", "hw1-", comment_template = "template", key_repo = "org/hw1-key", overwrite = TRUE
    )
  })

  expect_match(paste(msgs, collapse = ""), "Removing existing")
  expect_equal(sort(list.files(file.path(path, "repos"))), c("hw1-a", "hw1-b"))
  expect_equal(readLines(file.path(path, "comments", "hw1-a.md")), "template")
  expect_equal(list.files(file.path(path, "hw1-key")), character())
  expect_equal(readLines(file.path(path, "notes.md")), "notes")
})

test_that("org_grade_assignment() keeps existing folders when it fails before cloning", {
  local_grade_assignment_mocks()
  local_mocked_bindings(
    org_repos = function(...) character()
  )

  path = withr::local_tempdir()
  dir.create(file.path(path, "comments"))
  writeLines("graded", file.path(path, "comments", "hw1-a.md"))

  expect_error(org_grade_assignment(path, "org", "hw1-", overwrite = TRUE), "No repos found")
  expect_equal(readLines(file.path(path, "comments", "hw1-a.md")), "graded")
})

test_that("org_grade_assignment() rejects artifact names that are not folder names", {
  path = file.path(withr::local_tempdir(), "hw1")

  purrr::walk(
    list("html-output", c("html" = "a", "b"), c(".." = "a"), c("." = "a"), c("a/.." = "a"), c("a/b" = "a")),
    function(artifacts) {
      expect_error(
        org_grade_assignment(path, "org", "hw1-", artifacts = artifacts),
        "must be a named character vector"
      )
    }
  )

  expect_true(is_dir_name(c("html", "md")))
})

test_that("org_grade_assignment() confirms creating a folder named like the working directory", {
  local_grade_assignment_mocks()
  rlang::local_interactive(TRUE)

  wd = file.path(withr::local_tempdir(), "hw1")
  dir.create(wd)
  withr::local_dir(wd)

  asked = 0
  local_mocked_bindings(
    cli_yeah = function(...) {
      asked <<- asked + 1
      FALSE
    }
  )

  expect_null(org_grade_assignment("hw1", "org", "hw1-"))
  expect_equal(asked, 1)
  expect_false(dir.exists("hw1"))

  suppressMessages(org_grade_assignment("hw2", "org", "hw1-"))
  suppressMessages(org_grade_assignment(".", "org", "hw1-"))
  expect_equal(asked, 1)

  local_mocked_bindings(cli_yeah = function(...) TRUE)
  suppressMessages(org_grade_assignment("hw1", "org", "hw1-"))
  expect_true(dir.exists(file.path("hw1", "comments")))
})

test_that("org_grade_assignment() only warns about the folder name when not interactive", {
  local_grade_assignment_mocks()
  rlang::local_interactive(FALSE)

  wd = file.path(withr::local_tempdir(), "hw1")
  dir.create(wd)
  withr::local_dir(wd)

  msgs = capture_messages(org_grade_assignment("hw1", "org", "hw1-"))

  expect_match(paste(msgs, collapse = ""), "same name as the current directory")
  expect_true(dir.exists(file.path("hw1", "comments")))
})
