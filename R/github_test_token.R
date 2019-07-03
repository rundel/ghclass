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
#' @family authentication
#'
#' @export

test_github_token = function(token = get_github_token()) {
  res = purrr::safely(gh::gh)("/user", .token=token)

  status_msg(
    res,
    "Your github token is functioning correctly.",
    "Your github token failed to authenticate.",
  )
}
