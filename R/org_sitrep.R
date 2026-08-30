github_api_org = function(org) {
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org",
    org = org,
    .send_headers = c(Accept = "application/vnd.github.surtur-preview+json")
  )
}

#' @rdname org_perm
#' @export
#'
org_sitrep = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org)(org)

  if (failed(res)) {
    status_msg(res, fail = "Failed to retrieve details for org {.val {org}}.")
    return(invisible(org))
  }

  res_org = result(res)
  admins = org_admins(org)

  workflow = purrr::safely(github_api_org_workflow_permissions)(org)
  status_msg(workflow, fail = "Failed to retrieve default workflow permissions for org {.val {org}}.")
  workflow_perm = result(workflow)[["default_workflow_permissions"]]

  # Org settings are only reported to owners using a token with the admin:org scope
  settings = c(
    "collaborators", "total_private_repos", "default_repository_permission",
    "members_can_create_public_repositories", "members_can_create_private_repositories",
    "members_can_fork_private_repositories"
  )
  unavailable = purrr::map_lgl(settings, ~ is.null(res_org[[.x]]))

  setting = function(field) {
    value = res_org[[field]]
    if (is.null(value)) "unavailable" else value
  }

  repo_perm = res_org[["default_repository_permission"]]
  repo_perm_warn = NULL
  if (!is.null(repo_perm) && repo_perm != "none")
    repo_perm_warn = "members can currently view all repos in this org."

  workflow_perm_warn = NULL
  if (is.null(workflow_perm))
    workflow_perm_warn = "could not be retrieved, see the error above."
  else if (workflow_perm != "write")
    workflow_perm_warn = "this may prevent some GitHub actions from working correctly."

  allows_forking = res_org[["members_can_fork_private_repositories"]]
  forking_warn = NULL
  if (isTRUE(allows_forking))
    forking_warn = "by default members can currently fork private repos in this org."

  cli::cli_h1("{.strong {res_org$login} sitrep:}")
  cli::cli_ul()
  cli::cli_li(cli_kv("Admins", admins))
  cli::cli_li(cli_kv("Members", setting("collaborators")))
  cli::cli_li(cli_kv("Public repos", res_org$public_repos))
  cli::cli_li(cli_kv("Private repos", setting("total_private_repos")))
  cli::cli_li(cli_kv("Default repository permission", setting("default_repository_permission"), repo_perm_warn))
  cli::cli_li(cli_kv("Default workflow permissions", if (is.null(workflow_perm)) "unavailable" else workflow_perm, workflow_perm_warn))
  cli::cli_li(cli_kv("Members can create public repos", setting("members_can_create_public_repositories")))
  cli::cli_li(cli_kv("Members can create private repos", setting("members_can_create_private_repositories")))
  cli::cli_li(cli_kv("Members can fork private repos", setting("members_can_fork_private_repositories"), forking_warn))
  cli::cli_end()

  if (any(unavailable))
    cli::cli_alert_warning(
      "Some settings are unavailable, GitHub only reports them to org owners using a token with the {.code admin:org} scope."
    )

  invisible(res_org)
}
