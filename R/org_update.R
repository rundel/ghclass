github_api_org_update = function(
  org,
  default_repository_permission = NULL,
  members_can_create_repositories = NULL,
  members_can_create_private_repositories = NULL,
  members_can_create_public_repositories = NULL
) {
  gh::gh(
    "PATCH /orgs/:org",
    org = org,
    default_repository_permission = default_repository_permission,
    members_can_create_repositories = members_can_create_repositories,
    members_can_create_private_repositories = members_can_create_private_repositories,
    members_can_create_public_repositories  = members_can_create_public_repositories,
    .token = github_get_token(),
    .send_headers = c(Accept = "application/vnd.github.surtur-preview+json")
  )
}

#' @rdname org
#'
#' @param permission Default permission level members have for organization repositories:
#' * read - can pull, but not push to or administer this repository.
#' * write - can pull and push, but not administer this repository.
#' * admin - can pull, push, and administer this repository.
#' * none - no permissions granted by default.
#'
#' @export
#'
org_set_repo_permission = function(org, permission = c("none", "read", "write", "admin")) {
  arg_is_chr_scalar(org)
  permission = match.arg(permission)

  res = purrr::safely(github_api_org_update)(org, default_repository_permission = permission)

  status_msg(
    res,
    "Set org {.val org}'s repo permissions to {.val permission}.",
    "failed to set org {.val org}'s repo permissions."
  )

  invisible(result(res))
}
