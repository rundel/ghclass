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
