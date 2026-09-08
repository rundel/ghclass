#' @name github_token
#' @rdname github_token
#'
#' @title Tools for handling GitHub personal access tokens (PAT)
#'
#' @description
#' * `github_get_token` - returns the user's GitHub personal access token (PAT).
#'
#' * `github_set_token` - defines the user's GitHub PAT by setting the `GITHUB_PAT` environmental variable.
#' This value will persist until the session ends or `gihub_reset_token()` is called.
#'
#' * `github_reset_token` - removes the value stored in the `GITHUB_PAT` environmental variable.
#'
#' * `github_test_token` - checks if a PAT is valid by attempting to authenticate with the GitHub API.
#'
#' * `github_token_scopes` - returns a vector of scopes granted to the token.
#'
#' * `github_token_sitrep` - reports on the token: its type and source, the authenticated user,
#' the granted scopes (flagging any that ghclass needs but are missing), and the API rate limit.
#'
#' @param token Character. Either the literal token, or the path to a file containing the token.
#'
#' @details
#' This package looks for the personal access token (PAT) in the following places (in order):
#' * Value of `GITHUB_PAT` environmental variable.
#' * A credential returned by `gh::gh_token()`, such as `GITHUB_TOKEN`, a token stored with
#' `gitcreds`, or a Posit Connect viewer token.
#'
#' For additional details on creating a GitHub PAT see the usethis vignette on
#' [Managing Git(Hub) Credentials](https://usethis.r-lib.org/articles/articles/git-credentials.html).
#' For those who do not wish to read the entire article, the quick start method is to use:
#' * `usethis::create_github_token()` - to create the token and then,
#' * `gitcreds::gitcreds_set()` - to securely cache the token.
#'
#' ## Scopes
#'
#' A classic PAT needs the `repo` and `admin:org` scopes for ghclass to manage an organization's
#' repositories and teams, the `workflow` scope to add or modify files under `.github/workflows/`,
#' the `notifications` scope to use `repo_watch()`, `repo_ignore()`, and `repo_unwatch()`, and the
#' `delete_repo` scope to use `repo_delete()`. Note that `usethis::create_github_token()` does not
#' select `admin:org` by default. Fine-grained tokens do not report scopes, so
#' `github_token_sitrep()` cannot check them. GitHub does not support fine-grained
#' or GitHub App tokens for `repo_watch()`, `repo_ignore()`, or `repo_unwatch()`.
#'
#' ## GitHub Enterprise
#'
#' To use ghclass with a GitHub Enterprise Server instance, set the `GITHUB_API_URL`
#' environment variable to the instance's REST API endpoint, e.g.
#' `https://github.example.edu/api/v3`. This is the same variable used by [gh::gh()]
#' and is honored by all ghclass functions, including those that construct clone, push,
#' badge, and GraphQL URLs directly. Tokens are looked up per host, so
#' `gitcreds::gitcreds_set("https://github.example.edu")` can store an Enterprise token
#' alongside one for github.com.
#'
#' @return `github_get_token()` returns the current PAT as a character string with the `gh_pat`
#' class. See [gh::gh_token()] for additional details.
#'
#' `github_set_token()` and `github_reset_token()` return the result of `Sys.setenv()` and
#' `Sys.unsetenv()` respectively.
#'
#' `github_test_token()` invisibly returns a logical value, `TRUE` if the test passes,
#' `FALSE` if not.
#'
#' `github_token_scopes()` returns a character vector of granted scopes.
#'
#' `github_token_sitrep()` invisibly returns a list with the token's type, source, API url,
#' the authenticated user's login, the granted scopes (`NULL` when the token does not report
#' them), the scopes ghclass uses that are missing, and rate limit details.
#'
#' @examples
#' \dontrun{
#' github_test_token()
#'
#' github_token_scopes()
#'
#' github_token_sitrep()
#'
#' (pat = github_get_token())
#'
#' github_set_token("ghp_BadTokenBadTokenBadTokenBadTokenBadToken")
#' github_get_token()
#' github_test_token()
#'
#' github_set_token(pat)
#' }
#'
NULL


# Modified from usethis
# https://github.com/r-lib/usethis/blob/7c8e0049a1e40e6dcabbde069bb29576215a11b6/R/github_token.R
scold_for_renviron = function () {
  renviron_path = fs::path_expand(Sys.getenv("R_ENVIRON_USER", unset = ""))
  if (renviron_path == "")
    renviron_path = fs::path_home_r()

  renviron_path = fs::path(renviron_path, ".Renviron")

  if (!fs::file_exists(renviron_path))
    return(invisible())

  renviron_lines = readLines(renviron_path, encoding = "UTF-8", warn = FALSE)
  fishy_lines = grep("^GITHUB_(PAT|TOKEN).*=.+", renviron_lines, value = TRUE)

  if (length(fishy_lines) == 0)
    return(invisible())

  fishy_keys = gsub("=.*", "", fishy_lines)

  cli::cli_warn(
    c(
      "{.path {renviron_path}} defines environment {cli::qty(fishy_keys)} variable{?s}: {.val {fishy_keys}}.",
      i = paste(
        "This is no longer considered a best practice, and it is recommended that you instead use {.fun gitcreds::gitcreds_set}",
        "to securely store your PAT. Existing environement variables can be removed from {.path .Renviron} using",
        "{.fun usethis::edit_r_environ}."
      )
    ),
    .frequency = "once",
    .frequency_id = "warn_renviron_pat"
  )

  invisible()
}




#' @rdname github_token
#' @export
#'
github_get_token = function() {
  scold_for_renviron()

  # Give priority to env GITHUB_PAT if set (so we use PAT from github_set_token preferentially)
  token = Sys.getenv("GITHUB_PAT", "")
  class(token) = "gh_pat"

  if (token == "")
    token = gh::gh_token()

  if (file.exists("~/.github/token") & token == "") {
    cli::cli_abort( c(
      "File based token storage is no longer directly supported by ghclass:",
      "*" = "You can temporarily load this PAT using {.code github_set_token(\"~/.github/token\")}.",
      "*" = "or you can securely store your PAT using {.fun gitcreds::gitcreds}."
    ) )
  }

  if (token == "")
    cli::cli_abort( c(
      "Unable to locate a github token:",
      "*" = "You can temporarily set the token using {.fun github_set_token}",
      "*" = "or you can securely store your PAT using {.fun gitcreds::gitcreds}."
    ) )

  token
}

# Resolve a `token` argument that may be either a literal PAT or a path to a
# file containing one. Only an existing file is read, taking the first
# non-empty, whitespace-trimmed line.
read_token = function(token) {
  arg_is_chr_scalar(token)

  if (file.exists(token)) {
    lines = trimws(readLines(token, warn = FALSE))
    token = lines[nzchar(lines)][1]
  }

  token
}

#' @rdname github_token
#' @export
#'
github_set_token = function(token) {
  Sys.setenv(GITHUB_PAT = read_token(token))
}

#' @rdname github_token
#' @export
#'
github_reset_token = function() {
  Sys.unsetenv("GITHUB_PAT")
}

#' @rdname github_token
#' @export
#'
github_test_token = function(token = github_get_token()) {
  token = read_token(token)

  res = purrr::safely(github_api_user)(token)

  status_msg(
    res,
    "Your GitHub PAT authenticated correctly.",
    "Your GitHub PAT failed to authenticate.",
  )

  invisible(succeeded(res))
}

#' @rdname github_token
#' @export
#'
github_token_scopes = function(token = github_get_token()) {
  token = read_token(token)

  res = purrr::safely(github_api_user)(token)

  status_msg(
    res,
    fail = "Your GitHub PAT failed to authenticate."
  )

  parse_scopes(attr(result(res), "response")[["x-oauth-scopes"]])
}



#' @rdname github_token
#' @export
#'
github_token_sitrep = function(token = github_get_token()) {
  token_supplied = !missing(token)
  token = read_token(token)

  res = purrr::safely(github_api_user)(token)

  status_msg(res, fail = "Your GitHub PAT failed to authenticate.")

  if (failed(res))
    return(invisible(NULL))

  user = result(res)
  headers = attr(user, "response")

  scopes_header = headers[["x-oauth-scopes"]]
  scopes = parse_scopes(scopes_header)
  missing = names(ghclass_scopes)[!names(ghclass_scopes) %in% expand_scopes(scopes)]

  rate_limit = list(
    limit = as.integer(headers[["x-ratelimit-limit"]]),
    remaining = as.integer(headers[["x-ratelimit-remaining"]]),
    reset = as.POSIXct(as.numeric(headers[["x-ratelimit-reset"]]), origin = "1970-01-01")
  )

  info = list(
    type = token_type(token),
    source = token_source(token, supplied = token_supplied),
    api_url = Sys.getenv("GITHUB_API_URL", "https://api.github.com"),
    login = user[["login"]],
    scopes = if (is.null(scopes_header)) NULL else scopes,
    missing_scopes = if (is.null(scopes_header)) NULL else missing,
    rate_limit = rate_limit
  )

  cli::cli_h1("{.strong GitHub token sitrep:}")
  ul = cli::cli_ul()
  cli::cli_li(cli_kv("Token type", info$type))
  cli::cli_li(cli_kv("Token source", info$source))
  cli::cli_li(cli_kv("API url", info$api_url))
  cli::cli_li(cli_kv("Authenticated as", info$login))

  if (length(rate_limit$remaining) == 1) {
    reset = format(rate_limit$reset, "%H:%M:%S")
    cli::cli_li(cli_glue(
      "{cli::col_silver('Rate limit')}: {.val {rate_limit$remaining}} of {.val {rate_limit$limit}} ",
      "requests remaining, resets at {.val {reset}}."
    ))
  }

  if (is.null(scopes_header)) {
    cli::cli_li(cli_kv("Scopes", "not reported", "scopes cannot be checked for this token type."))
    cli::cli_end(ul)
    cli::cli_alert_info(paste0(
      "ghclass needs read and write access to repository administration, contents, issues, ",
      "pull requests, pages, workflows, and actions, read access to metadata, ",
      "and read and write access to organization administration and members."
    ))
    if (info$type %in% repo_subscription_unsupported_token_types)
      cli::cli_alert_warning(
        "GitHub does not support this token type for {.fun repo_watch}, {.fun repo_ignore}, or {.fun repo_unwatch}."
      )
  } else {
    cli::cli_li(paste0(cli::col_silver("Recommended scopes"), ":"))
    scope_ul = cli::cli_ul()
    purrr::walk(names(ghclass_scopes), function(scope) {
      warn = if (scope %in% missing) ghclass_scopes[[scope]] else NULL
      cli::cli_li(cli_kv(scope, !scope %in% missing, warn))
    })
    cli::cli_end(scope_ul)
    cli::cli_end(ul)
  }

  invisible(info)
}

# gh (>= 1.5) caches GET responses by url, so a recent /user response would be
# returned for whatever token is supplied (even an invalid one) unless the
# cache is bypassed
github_api_user = function(token) {
  withr::with_options(
    list(gh_cache = FALSE),
    gh::gh("/user", .token = token)
  )
}
