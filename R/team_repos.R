github_api_team_repos = function(org, team_slug) {
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/teams/:team_slug/repos",
    org = org,
    team_slug = team_slug
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
