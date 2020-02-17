github_api_team_add = function(repo, team_id, permission){
  gh::gh(
    "PUT /teams/:team_id/repos/:owner/:repo",
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
repo_add_team = function(repo, team, permission = c("push", "pull", "admin")) {

  permission = match.arg(permission)
  arg_is_chr(repo, team)

  repo = unique(repo)
  team = unique(team)

  org = unique(get_repo_owner(repo))

  d = tibble::tibble(team, repo)
  d = team_id_lookup(d, org)

  purrr::pwalk(
    d,
    function(team, id, repo) {
      if (missing_team(team, id, org)) return()

      res = purrr::safely(github_api_team_add)(
        repo = repo,
        team_id = id,
        permission = permission
      )

      status_msg(
        res,
        glue::glue("Added team {usethis::ui_value(team)} to repo {usethis::ui_value(repo)}."),
        glue::glue("Failed to add team {usethis::ui_value(team)} to repo {usethis::ui_value(repo)}.")
      )
    }
  )
}
