
github_api_repo_remove_team = function(
  org, team_slug, repo,
  permission = c("pull", "push", "admin", "maintain", "triage")
){
  permission = match.arg(permission)

  ghclass_api_v3_req(
    endpoint = "DELETE /orgs/:org/teams/:team_slug/repos/:owner/:repo",
    org = org,
    team_slug = team_slug,
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}


#' @rdname repo_user
#' @export
repo_remove_team = function(
  repo, team,
  team_type = c("name", "slug")
) {
  arg_is_chr(repo, team)
  team_type = match.arg(team_type)

  org = unique(get_repo_owner(repo))

  if (length(org) != 1) {
    cli_stop("Teams can only be removed from repositories within a single organization. ",
             "Requested orgs: {.val {org}}")
  }

  d = tibble::tibble(team, repo)
  d = dplyr::distinct(d)

  if (team_type == "name")
    d[["slug"]] = team_slug_lookup(org, d[["team"]])
  else
    d[["slug"]] = d[["team"]]

  check_team_slug(d[["slug"]])

  res = status_scope(
    "Removing teams from repos", nrow(d),
    done = "Removed {n_ok} of {total} team{?s} from repos",
    purrr::pmap(
      d,
      function(team, repo, slug) {
        if (is.na(slug)) {
          status_fail("Team {.val {team}} does not exist in org {.val {org}}.")
          return()
        }

        res = purrr::safely(github_api_repo_remove_team)(
          org = org,
          team_slug = slug,
          repo = repo
        )

        status_msg(
          res,
          "Removed team {.val {slug}} from repo {.val {repo}}.",
          "Failed to remove team {.val {slug}} from repo {.val {repo}}."
        )
      }
    )
  )

  invisible(res)
}
