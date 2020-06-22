#' @rdname github
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

#' @param token Character. Either the literal token path, or the path of a file containing the token.
#'
#' @rdname github
#' @export
#'
github_set_token = function(token) {
  arg_is_chr_scalar(token)

  token = as.character(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  Sys.setenv(GITHUB_PAT = token)
}

#' @rdname github
#' @export
#'
github_reset_token = function() {
  Sys.unsetenv("GITHUB_PAT")
}

#' @rdname github
#' @export
#'
github_test_token = function(token = github_get_token()) {
  arg_is_chr_scalar(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  res = purrr::safely(gh::gh)("/user", .token=token)

  status_msg(
    res,
    "Your GitHub PAT authenticated correctly.",
    "Your GitHub PAT failed to authenticate.",
  )
}
