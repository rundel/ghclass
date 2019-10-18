#' Get github login
#'
#' `github_whoami` returns the login of the authenticated user.
#'
#' @examples
#' \dontrun{
#' github_whoami()
#' }
#'
#' @export

github_whoami = function() {
  gh::gh_whoami()[["login"]]
}
