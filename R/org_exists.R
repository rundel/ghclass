github_api_get_org = function(org) {
  arg_is_chr_scalar(user)

  gh::gh(
    "/orgs/:org",
    org = org,
    .token = github_get_token()
  )
}

#' Check if organization exists
#'
#' `org_exists` returns TRUE if the supplied org(s) exist on GitHub and FALSE otherwise.
#'
#' @param org Character. Organization name to be checked.
#'
#' @return TRUE or FALSE
#'
#' @examples
#' \dontrun{
#' user_exists(c("rundel","ghclass-demo"))
#' }
#'
#' @export
#'
org_exists = function(org) {
  arg_is_chr(org)

  res = purrr::map(user, purrr::safely(github_api_get_org))
  purrr::map_lgl(res, succeeded)
}
