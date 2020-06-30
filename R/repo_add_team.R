github_api_team_add = function(
  org, team_slug, repo,
  permission = c("pull", "push", "admin", "maintain", "triage")
){
  permission = match.arg(permission)

  gh::gh(
    "PUT /orgs/:org/teams/:team_slug/repos/:owner/:repo",
    team_id = team_id,
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    permission = permission,
    .token = github_get_token()
  )
}

#' Add a team to a repository
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param team Character. Team name.
#' @param permission Character. Permission to be granted to team for repo ("push", "pull", or "admin"), defaults to "push".
#'
#' * pull - can pull, but not push to or administer this repository.
#' * push - can pull and push, but not administer this repository.
#' * admin - can pull, push and administer this repository.
#'
#' @export
repo_add_team = function(
  repo, team,
  permission = c("push", "pull", "admin", "maintain", "triage"),
  team_type = c("slug", "name")
) {
  arg_is_chr(repo, team)
  permission = match.arg(permission)
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

      res = purrr::safely(github_api_team_add)(
        org = org,
        team_slug = team_slug,
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
