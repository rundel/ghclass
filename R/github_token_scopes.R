#' Returns a vector of the o-auth scopes granted to the current PAT.
#'
#' @examples
#' \dontrun{
#' github_token_scopes()
#' }
#'
#' @export
#'
github_token_scopes = function(quiet = FALSE) {
  res = github_api_whoami()
  scopes = attr(res, "response")[["x-oauth-scopes"]]

  strsplit(scopes, ", ")[[1]]
}
