get_specific_teams = function(org, teams, strict = TRUE) {
  org_teams = get_teams(org)

  sub = teams %in% org_teams[["team"]]
  if (sum(sub) != length(teams) & strict) {
    missing = paste(teams[!sub], collapse=", ")
    usethis::ui_stop( paste0(
      "Team(s) {usethis::ui_value(missing)} do not exist ",
      "in org {usethis::ui_value(org)}."
    ) )
  }

  org_teams[org_teams$team %in% teams,]
}

team_id_lookup = function(d, org_teams) {
  d = merge(
    org_teams, d,
    by = "team", all.y = TRUE
  )

  missing_teams = d[["team"]][is.na(d[["id"]])]

  # This should not ever happen, get_specific_team should handle this
  stopifnot(length(missing_teams) == 0)

  d
}
