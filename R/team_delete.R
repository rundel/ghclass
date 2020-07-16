github_api_team_delete = function(org, team_slug) {
  ghclass_api_v3_req(
    endpoint = "DELETE /orgs/:org/teams/:team_slug",
    org = org,
    team_slug = team_slug
  )
}

#' @rdname team
#' @export
#'
team_delete = function(org, team, team_type = c("name", "slug"), prompt = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(team)
  arg_is_lgl_scalar(prompt)
  team_type = match.arg(team_type)

  if (prompt) {
    delete = cli_yeah("This command will delete the following teams permanently: {.val {team}}.")
    if (!delete) {
      return(invisible())
    }
  }

  if (team_type == "name")
    team = team_slug_lookup(org, team)

  check_team_slug(team)

  r = purrr::map(
    team,
    function(team) {

      if (is.na(team)) {
        cli::cli_alert_danger("Team {.val {team}} does not exist in org {.val {org}}.")
        return()
      }

      res = purrr::safely(github_api_team_delete)(org, team)

      status_msg(
        res,
        "Deleted team {.val {team}} from org {.val {org}}.",
        "Failed to delete team {.val {team}} from org {.val {org}}."
      )
    }
  )

  invisible(r)
}
