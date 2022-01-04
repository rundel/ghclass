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

    org = result(res)

    perm = org$default_repository_permission
    if (perm != "none")
      perm_warn = "members can currently view {.emph all} repos in this org."
    else
      perm_warn = NULL


    #if (perm != "none")
    #  perm = cli::col_red(perm)

    cli::cli_h1("{.strong {org$login} sitrep:}")
    cli::cli_ul()
    cli::cli_li(cli_kv("Admins", admins))
    cli::cli_li(cli_kv("Members", org$collaborators))
    cli::cli_li(cli_kv("Public repos", org$public_repos))
    cli::cli_li(cli_kv("Private repos", org$total_private_repos))
    cli::cli_li(
      cli_kv(
        "Default repository permission",
        perm,
        perm_warn
      )
    )
    cli::cli_li(cli_kv("Members can create public repos", org$members_can_create_public_repositories))
    cli::cli_li(cli_kv("Members can create private repos", org$members_can_create_private_repositories))
    cli::cli_end()

    invisible(org)
  }
}


