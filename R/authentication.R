#' Get github token
#'
#' \code{get_github_token} obtains the user's github authentication token.
#'
#' This function looks for the token in the following places (in order):
#'
#' * Value of \code{GITHUB_PAT} environmental variable.
#' * Value of \code{GITHUB_TOKEN} environmental variable.
#' * Contents of \code{~/.github/token} file.
#'
#' @examples
#' \dontrun{
#' get_github_token()
#' }
#'
#' @family authentication functions
#'
#' @export
#'
get_github_token = function() {
  token = usethis::github_token()
  if (token != "")
    return(invisible(token))

  if (file.exists("~/.github/token")) {
    set_github_token("~/.github/token")
    return(invisible(get_github_token()))
  }

  stop("Unable to locate github token, please use set_github_token",
       " or define the GITHUB_TOKEN environmental variable.")
}


#' Set github token
#'
#' `set_github_token` defines the user's github authentication token by
#' defining the `GITHUB_PAT` enivronmental variable. This value can then
#' be subsequently accessed using `get_github_token`.
#'
#' @param token character, either the path of a file contained the token or the actual token.
#'
#' @examples
#' \dontrun{
#' set_github_token("~/.github/token")
#' set_github_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' }
#'
#' @family authentication functions
#'
#' @export
#'
set_github_token = function(token) {
   token = as.character(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  Sys.setenv(GITHUB_PAT = token)
}

#' Reset github token
#'
#' `resset_github_token` removes any value stored in the `GITHUB_PAT`
#' enivronmental variable.
#'
#' @examples
#' \dontrun{
#' token = "0123456789ABCDEF0123456789ABCDEF01234567"
#' set_github_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' get_github_token() == token
#' reset_github_token()
#' get_github_token() == token
#' }
#'
#' @family authentication functions
#'
#' @export
#'
reset_github_token = function() {
  Sys.unsetenv("GITHUB_PAT")
}


#' Test github token
#'
#' test_github_token` checks if a token is valid by attempting to authenticate
#' with the GitHub api.
#'
#' @param token github api personal access token
#'
#' @examples
#' \dontrun{
#' test_github_token()
#' test_github_token("bad_token")
#' }
#'
#' @family authentication functions
#'
#' @export
#'
test_github_token = function(token = get_github_token()) {

  res = purrr::safely(gh)("/user", .token=token)

  status_msg(
    res,
    "Your github token is functioning correctly.",
    "Your github token failed to authenticate.",
  )
}

#' Use Temporary GitHub Access Token
#'
#' Temporarily change the `GITHUB_PAT` environmental variable for
#' GitHub authentication. Based on the `withr` interface.
#'
#' @param new Temporary GitHub access token
#' @param code Code to execute with the temporary token
#' @param .local_envir The environment to use for scoping.
#'
#' @details if `NA` is used the `GITHUB_PAT` environment variable will be unset.
#'
#' @export
#'
with_pat = function(new, code) {
  arg_is_chr_scalar(new)
  withr::with_envvar(c("GITHUB_PAT" = new), code, action = "replace")
}

#' @rdname with_pat
#' @export
#'
local_pat = function(new, .local_envir = parent.frame()) {
  arg_is_chr_scalar(new)
  withr::local_envvar(c("GITHUB_PAT" = new), action = "replace",
                      .local_envir = .local_envir)
}



