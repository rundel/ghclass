github_api_get_org = function(org) {
  arg_is_chr_scalar(org)

  gh::gh(
    "/orgs/:org",
    org = org,
    .token = github_get_token()
  )
}


#' @rdname org
#' @export
#'
org_exists = function(org) {
  arg_is_chr(org)

  res = purrr::map(org, purrr::safely(github_api_get_org))
  purrr::map_lgl(res, succeeded)
}
