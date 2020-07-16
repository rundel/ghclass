github_api_get_org = function(org) {
  arg_is_chr_scalar(org)

  ghclass_api_v3_req(
    endpoint = "/orgs/:org",
    org = org
  )
}


#' @rdname org_details
#' @export
#'
org_exists = function(org) {
  arg_is_chr(org)

  res = purrr::map(org, purrr::safely(github_api_get_org))
  purrr::map_lgl(res, succeeded)
}
