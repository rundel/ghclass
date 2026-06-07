github_api_team_members = function(org, team_slug, role = c("all", "member", "maintainer")) {
  role = match.arg(role)

  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/teams/:team_slug/members",
    org = org,
    team_slug = team_slug,
    role = role
  )
}

# TODO - good candidate for v4 upgrade

#' @rdname team_members
#' @export
#'
team_members = function(org, team = org_teams(org), team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)
  team_type = match.arg(team_type)

  slug = if (team_type == "name") team_slug_lookup(org, team) else team

  check_team_slug(slug)

  purrr::map2_dfr(
    team, slug,
    function(team, slug) {

      if (is.na(slug)) {
        res = NULL
      } else {
        res = purrr::safely(github_api_team_members)(org, slug)

        status_msg(
          res,
          fail = "Failed to retrieve team members for {.val {team}}."
        )
      }

      members = if (failed(res) | empty_result(res))
        character()
      else
        purrr::map_chr(result(res), "login")

      tibble::tibble(
        team = team,
        slug = slug,
        user = members
      )
    }
  )
}
