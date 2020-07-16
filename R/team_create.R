github_api_team_create = function(org, name, privacy) {
  ghclass_api_v3_req(
    endpoint = "POST /orgs/:org/teams",
    org=org, name=name, privacy=privacy
  )
}

#' @rdname team
#' @export
#'
team_create = function(
  org, team, prefix = "", suffix = "",
  privacy = c("secret","closed")
) {
  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr(team)

  team = unique(as.character(team))
  privacy = match.arg(privacy)

  team = paste0(prefix, team, suffix)

  org_teams = org_teams(org)

  new_teams = setdiff(team, org_teams)
  existing_teams = intersect(team, org_teams)

  if (length(existing_teams) > 0)
    cli::cli_alert_info("Skipping existing teams: {.val {existing_teams}}.")

  r = purrr::map(
    new_teams,
    function(team) {
      res = purrr::safely(github_api_team_create)(
        org = org, name = team, privacy = privacy
      )

      status_msg(
        res,
        "Created team {.val {team}} in org {.val {org}}.",
        "Failed to create team {.val {team}} in org {.val {org}}."
      )
    }
  )

  invisible(r)
}
