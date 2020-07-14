github_api_team_remove = function(org, team_slug, username) {
  gh::gh(
    "DELETE /orgs/:org/teams/:team_slug/memberships/:username",
    org = org,
    team_slug = team_slug,
    username = username,
    .token = github_get_token()
  )
}

#' @rdname team
#' @export
team_remove = function(org, user, team, team_type = c("name", "slug")) {

  arg_is_chr_scalar(org)
  arg_is_chr(user, team)

  if (team_type == "name")
    team = team_slug_lookup(org, team)

  check_team_slug(team)

  r = purrr::map2(
    user, team,
    function(user, team) {
      if (is.na(team)) {
        cli::cli_alert_danger("Team {.val {team}} does not exist in org {.val {org}}.")
        return(NULL)
      }

      res = purrr::safely(github_api_team_remove)(org, team, user)

      status_msg(
        res,
        "Removed user {.val {user}} from team {.val {team}}.",
        "Failed to remove user {.val {user}} from team {.val {team}}."
      )

      result(res)
    }
  )

  invisible(r)
}
