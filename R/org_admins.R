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

  if (failed(res)) {
    if (user_exists(org)) {
      ## In this case it is a user not an org, admin is just that user
      return(org)
    }

    cli_stop("Failed to retrieve admins for org {.val {org}}.")
  } else {
    purrr::map_chr(result(res), "login")
  }
}
