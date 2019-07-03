#' Get github token
#'
#' `get_github_token`` obtains the user's github authentication token.
#'
#' This function looks for the token in the following places (in order):
#'
#' * Value of `GITHUB_PAT` environmental variable.
#' * Value of `GITHUB_TOKEN` environmental variable.
#' * Contents of `~/.github/token` file.
#'
#' @examples
#' \dontrun{
#' get_github_token()
#' }
#'
#' @family authentication
#'
#' @export

get_github_token = function() {
  token = usethis::github_token()
  if (token != "")
    return(invisible(token))

  if (file.exists("~/.github/token")) {
    github_set_token("~/.github/token")
    return(invisible(get_github_token()))
  }

  stop("Unable to locate github token, please use github_set_token",
       " or define the GITHUB_TOKEN environmental variable.")
}
