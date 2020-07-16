github_api_org_members = function(org) {
  arg_is_chr_scalar(org)

  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/members",
    org = org
  )
}

#' @rdname org_members
#' @param include_admins Logical. Should admin users be included in the results.
#' @export
#'
org_members = function(org, filter = NULL, exclude = FALSE,
                      include_admins = TRUE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_org_members)(org)
  members = purrr::map_chr(result(res), "login")

  if (!include_admins)
    members = setdiff(members, org_admins(org))

  filter_results(members, filter, exclude)
}
