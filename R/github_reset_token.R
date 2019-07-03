#' Reset GitHub token
#'
#' `github_reset_token` removes any value stored in the `GITHUB_PAT`
#' enivronmental variable.
#'
#' @examples
#' \dontrun{
#' token = "0123456789ABCDEF0123456789ABCDEF01234567"
#' github_set_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' get_github_token() == token
#' github_reset_token()
#' get_github_token() == token
#' }
#'
#' @family authentication
#'
#' @export

github_reset_token = function() {
  Sys.unsetenv("GITHUB_PAT")
}
