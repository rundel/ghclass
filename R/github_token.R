#' @name github_token
#' @rdname github_token
#'
#' @title Tools for handling GitHub personal access tokens (PAT)
#'
#' @description
#' * `github_get_token` - returns the user's GitHub personal access token (PAT).
#'
#' * `github_set_token` - defines the user's GitHub PAT by setting the `GITHUB_PAT` environmental variable. This value can then
#'
#' * `github_reset_token` - removes the value stored in the `GITHUB_PAT` environmental variable.
#'
#' * `github_test_token` - checks if a PAT is valid by attempting to authenticate with the GitHub API
#'
#' @param token Character. Either the literal token path, or the path to a file containing the token.
#'
#' @details
#' This package looks for the personal access token (PAT) in the following places (in order):
#' * Value of `GITHUB_PAT` environmental variable.
#' * Value of `GITHUB_TOKEN` environmental variable.
#' * Contents of `~/.github/token` file.
#'
#' @examples
#' \dontrun{
#' github_test_token()
#'
#' pat = github_get_token()
#'
#' github_set_token("bad_token")
#' github_test_token()
#'
#' github_set_token(pat)
#' }
#'
NULL



#' @rdname github_token
#' @export
#'
github_get_token = function() {
  token = Sys.getenv("GITHUB_PAT", "")
  if (token == "")
    token = Sys.getenv("GITHUB_TOKEN", "")

  if (token != "")
    return(invisible(token))

  if (file.exists("~/.github/token")) {
    github_set_token("~/.github/token")
    return(invisible(github_get_token()))
  }

  cli_stop( paste0(
    "Unable to locate a github token, please use {.fun github_set_token}",
    " or define the {.field GITHUB_PAT} environmental variable in your {.file .Renviron} file."
  ) )
}

#' @rdname github_token
#' @export
#'
github_set_token = function(token) {
  arg_is_chr_scalar(token)

  token = as.character(token)

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
}
