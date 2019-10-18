github_api_get_org = function(org) {
  arg_is_chr_scalar(org)

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
#' @return Vector of logical values
#'
#' @examples
#' \dontrun{
#' org_exists(c("rundel","ghclass-demo"))
#' }
#'
#' @export
#'
org_exists = function(org) {
  arg_is_chr(org)

  res = purrr::map(org, purrr::safely(github_api_get_org))
  purrr::map_lgl(res, succeeded)
}
