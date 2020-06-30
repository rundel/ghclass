github_api_team_rename = function(id, new_name) {
  gh::gh(
    "PATCH /teams/:team_id",
    team_id = id,
    name = new_name,
    .token = github_get_token()
  )
}

github_api_team_update = function(
  org, team_slug,
  name = NULL, description = NULL,
  privacy = NULL, permission = NULL, parent_team_id = NULL
) {
  gh::gh(
    "PATCH /orgs/:org/teams/:team_slug",
    org = org,
    team_slug = team_slug,
    name = name,
    description = description,
    privacy = privacy,
    permission = permission,
    parent_team_id = parent_team_id,
    .token = github_get_token()
  )
}


#' @rdname team
#' @param new_team character, new team name.
#' @export
team_rename = function(org, team, new_team, team_type = c("slug", "name")) {
  arg_is_chr_scalar(org)
  arg_is_chr(team, new_team)
  team_type = match.arg(team_type)

  if (team_type == "name")
    team = team_slug_lookup(org, team)

  check_team_slug(team)

  purrr::walk2(
    team, new_team,
    function(team, new_team) {
      if (is.na(team))
        return()

      res = purrr::safely(github_api_team_update)(org, team, name = new_team)

      status_msg(
        res,
        "Renamed team {.val {team}} to {.val {new_team}}.",
        "Failed to rename team {.val {team}} to {.val {new_team}}."
      )
    }
  )
}
