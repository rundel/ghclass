github_api_team_update = function(
  org, team_slug,
  name = NULL, description = NULL,
  privacy = NULL, permission = NULL, parent_team_id = NULL
) {
  ghclass_api_v3_req(
    endpoint = "PATCH /orgs/:org/teams/:team_slug",
    org = org,
    team_slug = team_slug,
    name = name,
    description = description,
    privacy = privacy,
    permission = permission,
    parent_team_id = parent_team_id
  )
}


#' @rdname team
#' @param new_team character, new team name.
#' @export
team_rename = function(org, team, new_team, team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr(team, new_team)
  team_type = match.arg(team_type)

  if (team_type == "name") {
    slug = team_slug_lookup(org, team)
  } else {
    slug = team
  }

  check_team_slug(slug)

  purrr::pwalk(
    tibble::tibble(team, slug, new_team),
    function(team, slug, new_team) {
      if (is.na(slug)) {
        cli::cli_alert_danger("Team {.val {team}} does not exist.")
        return()
      }

      res = purrr::safely(github_api_team_update)(org, slug, name = new_team)

      status_msg(
        res,
        "Renamed team {.val {team}} to {.val {new_team}}.",
        "Failed to rename team {.val {team}} to {.val {new_team}}."
      )
    }
  )
}
