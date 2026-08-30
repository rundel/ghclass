fake_gh_error = function(status, message, doc = NULL, url = NULL, headers = list()) {
  body = character()
  if (!is.null(url))
    body = c(body, x = paste0("URL not found: <", url, ">"))
  if (!is.null(doc))
    body = c(body, i = paste0("Read more at <", doc, ">"))

  content = list(message = message, status = as.character(status))
  if (!is.null(doc))
    content[["documentation_url"]] = doc

  e = structure(
    list(
      message = paste0("GitHub API error (", status, "):  ", message),
      body = body,
      response_headers = headers,
      response_content = content
    ),
    class = c("github_error", paste0("http_error_", status), "rlang_error", "error", "condition")
  )

  list(result = NULL, error = e)
}

fake_token = function(chr) {
  paste0("ghp_", strrep(chr, 36))
}

test_that("parse_scopes() splits header values", {
  expect_equal(parse_scopes(NULL), character())
  expect_equal(parse_scopes(""), character())
  expect_equal(parse_scopes("repo"), "repo")
  expect_equal(parse_scopes("gist, read:org, repo, workflow"), c("gist", "read:org", "repo", "workflow"))
})

test_that("expand_scopes() includes implied scopes", {
  expect_equal(expand_scopes(character()), character())
  expect_true(all(c("admin:org", "write:org", "read:org") %in% expand_scopes("admin:org")))
  expect_true("read:org" %in% expand_scopes("write:org"))
  expect_false("write:org" %in% expand_scopes("read:org"))
  expect_true(all(c("repo", "public_repo") %in% expand_scopes("repo")))
  expect_equal(expand_scopes("workflow"), "workflow")
})

test_that("token_type() recognizes token prefixes", {
  expect_equal(token_type("ghp_abc"), "classic personal access token")
  expect_equal(token_type("github_pat_abc"), "fine-grained personal access token")
  expect_equal(token_type("gho_abc"), "OAuth access token")
  expect_equal(token_type("ghu_abc"), "GitHub App user access token")
  expect_equal(token_type("ghs_abc"), "GitHub App installation access token")
  expect_equal(token_type("0123456789abcdef"), "unknown")
})

test_that("token_source() reports where a token came from", {
  withr::with_envvar(c(GITHUB_PAT = fake_token("a"), GITHUB_TOKEN = NA), {
    expect_equal(token_source(fake_token("a")), "GITHUB_PAT environment variable")
    expect_equal(token_source(fake_token("b")), "supplied directly")
  })

  withr::with_envvar(c(GITHUB_PAT = NA, GITHUB_TOKEN = fake_token("a")), {
    expect_equal(token_source(fake_token("a")), "GITHUB_TOKEN environment variable")
  })
})

test_that("missing_scope_hint() compares token and endpoint scopes", {
  expect_null(missing_scope_hint(NULL))
  expect_null(missing_scope_hint(list("x-accepted-oauth-scopes" = "admin:org")))
  expect_null(missing_scope_hint(list("x-oauth-scopes" = "repo", "x-accepted-oauth-scopes" = "")))
  expect_null(missing_scope_hint(list("x-oauth-scopes" = "admin:org", "x-accepted-oauth-scopes" = "read:org")))
  expect_null(missing_scope_hint(list(
    "x-oauth-scopes" = "admin:org, repo",
    "x-accepted-oauth-scopes" = "admin:org, read:org, repo, user, write:org"
  )))

  expect_equal(
    missing_scope_hint(list("x-oauth-scopes" = "gist, read:org, repo, workflow", "x-accepted-oauth-scopes" = "admin:org")),
    "admin:org (token has gist, read:org, repo, workflow)"
  )
  expect_equal(
    missing_scope_hint(list("x-oauth-scopes" = "gist", "x-accepted-oauth-scopes" = "admin:org, read:org")),
    "one of admin:org, read:org (token has gist)"
  )
  expect_equal(
    missing_scope_hint(list("x-oauth-scopes" = "", "x-accepted-oauth-scopes" = "repo")),
    "repo (token has none)"
  )
})

test_that("error_msg() extracts details from gh errors", {
  res = fake_gh_error(
    403, "You must be an org admin.", doc = "https://docs.github.com/x",
    headers = list("x-oauth-scopes" = "repo", "x-accepted-oauth-scopes" = "admin:org")
  )
  msg = error_msg(res)
  expect_equal(as.vector(msg), "GitHub API error (403): You must be an org admin.")
  expect_null(attr(msg, "msg"))
  expect_equal(attr(msg, "doc"), "https://docs.github.com/x")
  expect_null(attr(msg, "404"))
  expect_equal(attr(msg, "scopes"), "admin:org (token has repo)")

  res = fake_gh_error(
    404, "Not Found", doc = "https://docs.github.com/y", url = "https://api.github.com/orgs/nope",
    headers = list("x-oauth-scopes" = "repo", "x-accepted-oauth-scopes" = "admin:org, repo")
  )
  msg = error_msg(res)
  expect_equal(as.vector(msg), "GitHub API error (404): Not Found")
  expect_equal(attr(msg, "404"), "https://api.github.com/orgs/nope")
  expect_null(attr(msg, "scopes"))
})

test_that("error_msg() parses legacy multiline gh messages", {
  e = structure(
    list(message = paste0(
      "GitHub API error (404): Not Found\n",
      "  Message: Not Found\n",
      "  URL not found: https://api.github.com/orgs/nope\n",
      "  Read more at https://docs.github.com/y"
    )),
    class = c("github_error", "http_error_404", "error", "condition")
  )
  msg = error_msg(list(result = NULL, error = e))
  expect_equal(as.vector(msg), "GitHub API error (404): Not Found")
  expect_null(attr(msg, "msg"))
  expect_equal(attr(msg, "doc"), "https://docs.github.com/y")
  expect_equal(attr(msg, "404"), "https://api.github.com/orgs/nope")
  expect_null(attr(msg, "scopes"))
})

test_that("error_msg() passes through non-API errors", {
  res = purrr::safely(stop)("boom")
  expect_equal(error_msg(res), "boom")
  expect_equal(error_bullets(res), c(x = "boom"))
})

test_that("error_msg_tree() and error_bullets() include the details", {
  local_reproducible_output()

  res = fake_gh_error(
    403, "Forbidden.", doc = "https://docs.github.com/x",
    headers = list("x-oauth-scopes" = "repo", "x-accepted-oauth-scopes" = "admin:org")
  )

  tree = error_msg_tree(error_msg(res))
  expect_true(any(grepl("GitHub API error (403): Forbidden.", tree, fixed = TRUE)))
  expect_true(any(grepl("API docs: https://docs.github.com/x", tree, fixed = TRUE)))
  expect_true(any(grepl("Missing scope: admin:org (token has repo)", tree, fixed = TRUE)))

  bullets = error_bullets(res)
  expect_equal(names(bullets), c("x", "i", "i"))
  expect_equal(bullets[["x"]], "GitHub API error (403): Forbidden.")
  expect_true(any(grepl("API docs: {.url https://docs.github.com/x}", bullets, fixed = TRUE)))
  expect_true(any(grepl("Missing scope: admin:org (token has repo)", bullets, fixed = TRUE)))
})

test_that("error_bullets() escapes braces for cli", {
  res = fake_gh_error(422, "Bad {thing}")
  bullets = error_bullets(res)
  expect_equal(bullets[["x"]], "GitHub API error (422): Bad {{thing}}")
  expect_equal(cli::format_inline(bullets[["x"]]), "GitHub API error (422): Bad {thing}")
})
