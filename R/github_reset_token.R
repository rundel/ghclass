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
#' @family authentication
#'
#' @export

reset_github_token = function() {
  Sys.unsetenv("GITHUB_PAT")
}
