github_api_team_members = function(org, team_slug, role = c("all", "member", "maintainer")) {
  role = match.arg(role)

  gh::gh(
    "GET /orgs/:org/teams/:team_slug/members",
    org = org,
    team_slug = team_slug,
    role = role,
    .token = github_get_token(),
    .limit = github_get_api_limit()
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

  if (team_type == "name")
    team = team_slug_lookup(org, team)

  check_team_slug(team)

  purrr::map_dfr(
    team,
    function(team) {

      if (is.na(team)) {
        res = NULL
      } else {
        res = purrr::safely(github_api_team_members)(org, team)

        status_msg(
          res,
          fail = "Failed to retrieve team members for {.val {team}}."
        )
      }

      if (failed(res) | empty_result(res)) {
        tibble::tibble(
          team = character(),
          user = character()
        )
      } else {
        tibble::tibble(
          team = team,
          user = purrr::map_chr(result(res), "login"),
        )
      }
    }
  )
}
