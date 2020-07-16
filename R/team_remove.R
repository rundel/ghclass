github_api_team_remove = function(org, team_slug, username) {
  ghclass_api_v3_req(
    endpoint = "DELETE /orgs/:org/teams/:team_slug/memberships/:username",
    org = org,
    team_slug = team_slug,
    username = username
  )
}

#' @rdname team_members
#' @export
team_remove = function(org, user, team, team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr(user, team)
  team_type = match.arg(team_type)

  if (team_type == "name")
    slug = team_slug_lookup(org, team)
  else
    slug = team

  check_team_slug(slug)

  r = purrr::pmap(
    tibble::tibble(user, team, slug),
    function(user, team, slug) {
      if (is.na(slug)) {
        cli::cli_alert_danger("Team {.val {team}} does not exist in org {.val {org}}.")
        return(NULL)
      }

      res = purrr::safely(github_api_team_remove)(org, slug, user)

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
