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

  slug = if (team_type == "name") team_slug_lookup(org, team) else team

  check_team_slug(slug)

  purrr::map2_dfr(
    team, slug,
    function(team, slug) {
      if (is.na(slug))
        res = NULL
      else
        res = purrr::safely(github_api_team_repos)(org, slug)

      repos = if (failed(res) | empty_result(res))
        character()
      else
        purrr::map_chr(result(res), "full_name")

      tibble::tibble(
        team = team,
        slug = slug,
        repo = repos
      )
    }
  )
}
