github_api_team_invite = function(org, team_slug, username, role = c("member", "maintainer")) {
  role = match.arg(role)

  gh::gh(
    "PUT /orgs/:org/teams/:team_slug/memberships/:username",
    org = org,
    team_slug = team_slug,
    username = username,
    role = role,
    .token = github_get_token()
  )
}

#' @rdname team
#' @export
team_invite = function(org, user, team, team_type = c("slug", "name")) {
  arg_is_chr_scalar(org)
  arg_is_chr(user, team)
  team_type = match.arg(team_type)


  if (team_type == "name")
    team = team_slug_lookup(org, team)

  check_team_slug(team)

  r = purrr::map2(
    user, team,
    function(user, team) {
      if (is.na(team)) {
        cli::cli_alert_danger("Failed to find team {.val {team}} in org {.val {org}}.")
        return(NULL)
      }

      res = purrr::safely(github_api_team_invite)(org, team, user)

      status_msg(
        res,
        "Added user {.val {user}} to team {.val {team}}.",
        "Failed to add user {.val {user}} to team {.val {team}}."
      )

      result(res)
    }
  )

  invisible(r)
}
