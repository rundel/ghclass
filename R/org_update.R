github_api_org_update = function(
  org,
  default_repository_permission = NULL,
  members_can_create_repositories = NULL,
  members_can_create_private_repositories = NULL,
  members_can_create_public_repositories = NULL
) {
  ghclass_api_v3_req(
    endpoint = "PATCH /orgs/:org",
    org = org,
    default_repository_permission = default_repository_permission,
    members_can_create_repositories = members_can_create_repositories,
    members_can_create_private_repositories = members_can_create_private_repositories,
    members_can_create_public_repositories  = members_can_create_public_repositories,
    .send_headers = c(Accept = "application/vnd.github.surtur-preview+json")
  )
}

#' @rdname org_perm
#' @export
#'
org_set_repo_permission = function(org, permission = c("none", "read", "write", "admin")) {
  arg_is_chr_scalar(org)
  permission = match.arg(permission)

  res = purrr::safely(github_api_org_update)(org, default_repository_permission = permission)

  status_msg(
    res,
    "Set org {.val {org}}'s repo permissions to {.val {permission}}.",
    "failed to set org {.val {org}}'s repo permissions."
  )

  invisible(result(res))
}
