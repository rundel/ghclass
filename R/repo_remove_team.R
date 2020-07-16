
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
    cli_stop("Repositories can only be added to one organization at a time.",
             "Requested orgs: {.val {org}}")
  }

  d = tibble::tibble(team, repo)
  d = dplyr::distinct(d)

  if (team_type == "name")
    d[["team"]] = team_slug_lookup(org, d[["team"]])

  check_team_slug(d[["team"]])

  purrr::pwalk(
    d,
    function(team, repo) {
      if (is.na(team))
        return()

      res = purrr::safely(github_api_repo_remove_team)(
        org = org,
        team_slug = team,
        repo = repo
      )

      status_msg(
        res,
        "Removed team {.val {team}} from repo {.val {repo}}.",
        "Failed to remove team {.val {team}} from repo {.val {repo}}."
      )
    }
  )
}
