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
#' @param token Character. Either the literal token, or the path to a file containing the token.
#'
#' @details
#' This package looks for the personal access token (PAT) in the following places (in order):
#' * Value of `GITHUB_PAT` environmental variable.
#' * Any GitHub PAT token(s) stored with `gitcreds` via `gitcreds_set()`.
#'
#' For additional details on creating a GitHub PAT see the usethis vignette on
#' [Managing Git(Hub) Credentials](https://usethis.r-lib.org/articles/articles/git-credentials.html).
#' For those who do not wish to read the entire article, the quick start method is to use:
#' * `usethis::create_github_token()` - to create the token and then,
#' * `gitcreds::gitcreds_set()` - to securely cache the token.
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
#' @examples
#' \dontrun{
#' github_test_token()
#'
#' github_token_scopes()
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

#' @rdname github_token
#' @export
#'
github_set_token = function(token) {
  arg_is_chr_scalar(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  Sys.setenv(GITHUB_PAT = token)
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
  arg_is_chr_scalar(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  res = purrr::safely(gh::gh)("/user", .token = token)

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
  arg_is_chr_scalar(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  res = purrr::safely(gh::gh)("/user", .token = token)

  status_msg(
    res,
    fail = "Your GitHub PAT failed to authenticate."
  )

  strsplit(
    attr(result(res), "response")[["x-oauth-scopes"]],
    ", "
  )[[1]]
}


