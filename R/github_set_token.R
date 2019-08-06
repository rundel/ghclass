#' Set github token
#'
#' `github_set_token` defines the user's github authentication token by
#' defining the `GITHUB_PAT` enivronmental variable. This value can then
#' be subsequently accessed using `github_get_token`.
#'
#' @param token character, either the path of a file contained the token or the actual token.
#'
#' @examples
#' \dontrun{
#' github_set_token("~/.github/token")
#' github_set_token("0123456789ABCDEF0123456789ABCDEF01234567")
#' }
#'
#' @aliases set_github_token
#'
#' @export

github_set_token = function(token) {
  token = as.character(token)

  if (file.exists(token))
    token = readLines(token, warn=FALSE)

  Sys.setenv(GITHUB_PAT = token)
}
