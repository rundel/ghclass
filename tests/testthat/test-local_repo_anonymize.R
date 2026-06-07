roster_fixture = function() {
  tibble::tibble(
    last = c("Smith", "Jones"),
    first = c("Alice", "Bob"),
    email = c("alice.smith@duke.edu", "bob.jones@duke.edu"),
    netid = c("as99", "bj12"),
    github = c("alicesmith", "bobby"),
    name = c("Alice Smith", "Bob Jones")
  )
}

# Build a grading-project-style fixture: <root>/repos/<repo> with git history.
grading_fixture = function(dir) {
  repo = fs::path(dir, "repos", "hw1-team01")
  fs::dir_create(repo)

  writeLines(
    c(
      "# Homework 1",
      "* Alice Smith - alice.smith@duke.edu",
      "* Bob Jones (bj12) - bob.jones@duke.edu",
      "",
      "The word smithy must be preserved."
    ),
    fs::path(repo, "README.md")
  )
  writeBin(as.raw(c(1L, 0L, 2L, 0L, 255L)), fs::path(repo, "logo.png"))

  art = fs::path(dir, "html")
  fs::dir_create(art)
  writeLines('<meta name="author" content="Alice Smith">', fs::path(art, "hw1-team01.html"))

  gert::git_init(repo)
  gert::git_add("README.md", repo = repo)
  gert::git_commit(
    "initial commit by Alice", repo = repo,
    author = gert::git_signature("Alice Smith", "as99@machine.stat.duke.edu")
  )
  writeLines("more", fs::path(repo, "notes.txt"))
  gert::git_add("notes.txt", repo = repo)
  gert::git_commit(
    "Bob Jones adds notes", repo = repo,
    author = gert::git_signature("Bob Jones (bj12)", "bob.jones@duke.edu")
  )

  list(root = dir, repo = repo, html = art)
}


test_that("text PII is replaced and non-PII is preserved", {
  skip_if_not_installed("gert")

  root = withr::local_tempdir()
  grading_fixture(root)
  out = paste0(root, "_anon")
  withr::defer(unlink(out, recursive = TRUE))

  suppressWarnings(
    local_repo_anonymize(root, roster = roster_fixture(), git_history = "keep", prompt = FALSE, output = out)
  )

  readme = readr::read_file(fs::path(out, "repos", "hw1-team01", "README.md"))
  expect_false(grepl("Alice|Smith|alice.smith@duke.edu|as99|alicesmith", readme))
  expect_false(grepl("Bob|Jones|bob.jones@duke.edu|bj12", readme))
  expect_match(readme, "student_01")
  expect_match(readme, "student_02")
  expect_match(readme, "smithy")

  html = readr::read_file(fs::path(out, "html", "hw1-team01.html"))
  expect_false(grepl("Alice Smith", html))
  expect_match(html, "student_01")
})

test_that("binary files are left untouched", {
  skip_if_not_installed("gert")

  root = withr::local_tempdir()
  grading_fixture(root)
  out = paste0(root, "_anon")
  withr::defer(unlink(out, recursive = TRUE))

  suppressWarnings(
    local_repo_anonymize(root, roster = roster_fixture(), git_history = "keep", prompt = FALSE, output = out)
  )

  png = readBin(fs::path(out, "repos", "hw1-team01", "logo.png"), "raw", 10)
  expect_identical(png, as.raw(c(1L, 0L, 2L, 0L, 255L)))
})

test_that("only files matching types are scrubbed", {
  skip_if_not_installed("gert")

  root = withr::local_tempdir()
  grading_fixture(root)
  writeLines("contact Alice Smith", fs::path(root, "repos", "hw1-team01", "extra.log"))

  out1 = paste0(root, "_a")
  withr::defer(unlink(out1, recursive = TRUE))
  suppressWarnings(
    local_repo_anonymize(root, roster = roster_fixture(), git_history = "keep", prompt = FALSE, output = out1)
  )
  expect_match(readr::read_file(fs::path(out1, "repos", "hw1-team01", "extra.log")), "Alice Smith")

  out2 = paste0(root, "_b")
  withr::defer(unlink(out2, recursive = TRUE))
  suppressWarnings(
    local_repo_anonymize(root, roster = roster_fixture(), types = ".log", git_history = "keep", prompt = FALSE, output = out2)
  )
  expect_false(grepl("Alice Smith", readr::read_file(fs::path(out2, "repos", "hw1-team01", "extra.log"))))
})

test_that("the original directory is untouched when writing to a copy", {
  skip_if_not_installed("gert")

  root = withr::local_tempdir()
  fx = grading_fixture(root)
  out = paste0(root, "_anon")
  withr::defer(unlink(out, recursive = TRUE))

  local_repo_anonymize(root, roster = roster_fixture(), git_history = "delete", prompt = FALSE, output = out)

  orig = readr::read_file(fs::path(fx$repo, "README.md"))
  expect_match(orig, "Alice Smith")
  expect_true(fs::dir_exists(fs::path(fx$repo, ".git")))
  log = gert::git_log(repo = fx$repo, max = 5)
  expect_true(any(grepl("Alice", log$author)))
})

test_that("delete removes the .git directory entirely", {
  skip_if_not_installed("gert")

  root = withr::local_tempdir()
  grading_fixture(root)
  out = paste0(root, "_anon")
  withr::defer(unlink(out, recursive = TRUE))

  res = local_repo_anonymize(root, roster = roster_fixture(), git_history = "delete", prompt = FALSE, output = out)

  arepo = fs::path(out, "repos", "hw1-team01")
  expect_false(fs::dir_exists(fs::path(arepo, ".git")))
  # working tree is still scrubbed
  expect_false(grepl("Alice Smith", readr::read_file(fs::path(arepo, "README.md"))))
  expect_true(all(res$git$deleted))
})

test_that("keep retains git history and warns", {
  skip_if_not_installed("gert")

  root = withr::local_tempdir()
  grading_fixture(root)
  out = paste0(root, "_anon")
  withr::defer(unlink(out, recursive = TRUE))

  expect_warning(
    local_repo_anonymize(root, roster = roster_fixture(), git_history = "keep", prompt = FALSE, output = out),
    "not safe to distribute"
  )

  arepo = fs::path(out, "repos", "hw1-team01")
  expect_true(fs::dir_exists(fs::path(arepo, ".git")))
  log = gert::git_log(repo = arepo, max = 5)
  expect_true(any(grepl("Alice|Bob", log$author)))
})

test_that("cols accepts tidyselect expressions", {
  map = anon_build_map(roster_fixture(), rlang::quo(c(netid, email)))
  expect_setequal(
    sort(map$literal),
    sort(c("as99", "bj12", "alice.smith@duke.edu", "bob.jones@duke.edu"))
  )
})

test_that("missing default columns are tolerated", {
  roster = roster_fixture()[, c("name", "email")]
  map = anon_build_map(
    roster,
    rlang::quo(tidyselect::any_of(c("name", "first", "last", "email", "netid", "github")))
  )
  expect_setequal(sort(unique(map$token)), c("student_01", "student_02"))
})

test_that("shared values across students warn and collapse to one token", {
  roster = tibble::tibble(
    name = c("Alice Smith", "Bob Smith"),
    first = c("Alice", "Bob"),
    last = c("Smith", "Smith")
  )
  expect_warning(
    anon_build_map(roster, rlang::quo(c(name, first, last))),
    "multiple students"
  )
  map = suppressWarnings(anon_build_map(roster, rlang::quo(c(name, first, last))))
  expect_equal(sum(map$literal == "Smith"), 1L)
})

test_that("short values are dropped with a warning", {
  roster = tibble::tibble(name = c("Al", "X"))
  expect_warning(
    anon_build_map(roster, rlang::quo(name)),
    "shorter than 2"
  )
  map = suppressWarnings(anon_build_map(roster, rlang::quo(name)))
  expect_false("X" %in% map$literal)
})
