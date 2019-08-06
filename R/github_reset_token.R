#' Reset GitHub token
#'
#' `github_reset_token` removes any value stored in the `GITHUB_PAT`
#' enivronmental variable.
#'
#' @examples
#' \dontrun{
#' token = "0123456789ABCDEF0123456789ABCDEF01234567"
#' github_set_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' github_get_token() == token
#' github_reset_token()
#' github_get_token() == token
#' }
#'

#' @export

github_reset_token = function() {
  Sys.unsetenv("GITHUB_PAT")
}
