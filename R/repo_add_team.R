github_api_team_add = function(
  org, team_slug, repo,
  permission = c("pull", "push", "admin", "maintain", "triage")
){
  permission = match.arg(permission)

  ghclass_api_v3_req(
    endpoint = "PUT /orgs/:org/teams/:team_slug/repos/:owner/:repo",
    org = org,
    team_slug = team_slug,
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    permission = permission
  )
}


#' @rdname repo_user
#' @param team Character. Slug or name of team to add.
#' @param team_type Character. Either "slug" if the team names are slugs or "name" if full team names are provided.
#' @export
repo_add_team = function(
  repo, team,
  permission = c("push", "pull", "admin", "maintain", "triage"),
  team_type = c("name", "slug")
) {
  arg_is_chr(repo, team, allow_empty = FALSE)
  permission = match.arg(permission)
  team_type = match.arg(team_type)

  org = unique(get_repo_owner(repo))

  if (length(org) != 1) {
    cli_stop("Repositories can only be added to one organization at a time. ",
             "Requested orgs: {.val {org}}")
  }

  repo = unique(repo)
  team = unique(team)

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

      res = purrr::safely(github_api_team_add)(
        org = org,
        team_slug = team,
        repo = repo,
        permission = permission
      )

      status_msg(
        res,
        "Added team {.val {team}} to repo {.val {repo}} with {.val {permission}} access.",
        "Failed to add team {.val {team}} to repo {.val {repo}}."
      )
    }
  )
}
