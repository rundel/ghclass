github_api_org_pending = function(owner){
  arg_is_chr_scalar(owner)
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:owner/invitations",
    owner = owner
  )
}

#' @rdname org_members
#' @export
#'
org_pending = function(org, filter = NULL, exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_org_pending)(org)
  status_msg(
    res,
    fail = "Failed to retrieve pending members for org {.val {org}}"
  )

  invite = purrr::map(result(res), "login")
  invite = purrr::flatten_chr(invite)
  filter_results(invite, filter, exclude)
}
