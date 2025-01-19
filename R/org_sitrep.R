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
  res = purrr::safely(github_api_org)(org)

  if (failed(res)) {
    status_msg(res, fail = "Failed to find org {.val org}.")
    invisible(org)
  } else {
    admins = org_admins(org)
    workflow_perm = org_workflow_permissions(org)

    res_org = result(res)

    repo_perm = res_org$default_repository_permission
    if (repo_perm != "none")
      repo_perm_warn = "members can currently view {.emph all} repos in this org."
    else
      repo_perm_warn = NULL


    if (workflow_perm != "write")
      workflow_perm_warn = "this may prevent some GitHub actions from working correctly."
    else
      workflow_perm_warn = NULL

    #if (perm != "none")
    #  perm = cli::col_red(perm)

    cli::cli_h1("{.strong {res_org$login} sitrep:}")
    cli::cli_ul()
    cli::cli_li(cli_kv("Admins", admins))
    cli::cli_li(cli_kv("Members", res_org$collaborators))
    cli::cli_li(cli_kv("Public repos", res_org$public_repos))
    cli::cli_li(cli_kv("Private repos", res_org$total_private_repos))
    cli::cli_li(
      cli_kv(
        "Default repository permission",
        repo_perm,
        repo_perm_warn
      )
    )
    cli::cli_li(
      cli_kv(
        "Default workflow permissions",
        workflow_perm,
        workflow_perm_warn
      )
    )
    cli::cli_li(cli_kv("Members can create public repos", res_org$members_can_create_public_repositories))
    cli::cli_li(cli_kv("Members can create private repos", res_org$members_can_create_private_repositories))
    cli::cli_end()

    invisible(res_org)
  }
}


