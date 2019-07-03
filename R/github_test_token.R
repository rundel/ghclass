#' Test github token
#'
#' github_test_token` checks if a token is valid by attempting to authenticate
#' with the GitHub api.
#'
#' @param token github api personal access token
#'
#' @examples
#' \dontrun{
#' github_test_token()
#' github_test_token("bad_token")
#' }
#'
#' @family authentication
#'
#' @export

github_test_token = function(token = github_get_token()) {
  res = purrr::safely(gh::gh)("/user", .token=token)

  status_msg(
    res,
    "Your github token is functioning correctly.",
    "Your github token failed to authenticate.",
  )
}
