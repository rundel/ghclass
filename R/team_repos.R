github_api_team_repos = function(org, team_slug) {
  gh::gh(
    "GET /orgs/:org/teams/:team_slug/repos",
    team_slug = team_slug,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}


#' @rdname team_members
#' @export
#'
team_repos = function(org, team = org_teams(org), team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)
  team_type = match.arg(team_type)

  if (team_type == "name")
    team = team_slug_lookup(org, team)

  check_team_slug(team)

  purrr::map_dfr(
    team,
    function(team) {
      if (is.na(team))
        res = NULL
      else
        res = purrr::safely(github_api_team_repos)(org, team)

      if (failed(res) | empty_result(res)) {
        tibble::tibble(
          team = character(),
          repo = character()
        )
      } else {
        tibble::tibble(
          team = team,
          repo = purrr::map_chr(result(res), "full_name")
        )
      }
    }
  )
}
