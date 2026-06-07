github_api_org_admins = function(owner){
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:owner/members",
    owner = owner,
    role = "admin"
  )
}

#' @rdname org_members
#' @export
#'
org_admins = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org_admins)(owner = org)

  if (succeeded(res))
    return(purrr::map_chr(result(res), "login"))

  # A user account has no org-members endpoint, so the call above fails and the
  # user is their own sole admin. Use user_type() rather than user_exists()
  # (which is also TRUE for organizations) so a transient failure on a real org
  # errors instead of being misreported as a user.
  if (identical(user_type(org), "User"))
    return(org)

  cli_stop("Failed to retrieve admins for org {.val {org}}.")
}
