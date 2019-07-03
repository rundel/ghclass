github_api_create_team = function(org, name, privacy) {
  gh::gh(
    "POST /orgs/:org/teams",
    org=org, name=name, privacy=privacy,
    .token = github_get_token()
  )
}

#' Create team(s)
#'
#' `create_team` creates teams in your GitHub organization
#'
#' @param org character, name of the GitHub organization
#' @param team character, listing one or more teams
#' @param privacy character, level of privacy of teams, closed (visible to all
#' members of the organization) or secret (only visible to organization owners
#' and members of a team), default is closed
#'
#' @examples
#' \dontrun{
#' create_team("ghclass",c("team01","team01"))
#' }
#'
#' @family github organization team related functions
#'
#' @export
#'
create_team = function(org, team, privacy = c("closed","secret")) {
  team = unique(as.character(team))
  privacy = match.arg(privacy)

  org_teams = get_teams(org)[["team"]]

  new_teams = setdiff(team, org_teams)
  existing_teams = intersect(team, org_teams)

  if (length(existing_teams) > 0)
    usethis::ui_info("Skipping existing teams: {usethis::ui_value(existing_teams)}.")

  purrr::walk(
    new_teams,
    function(team) {
      res = purrr::safely(github_api_create_team)(
        org=org, name=team, privacy=privacy
      )

      status_msg(
        res,
        glue::glue("Added team {usethis::ui_value(team)} to org {usethis::ui_value(org)}."),
        glue::glue("Failed to add team {usethis::ui_value(team)} to org {usethis::ui_value(org)}."),
      )
    }
  )
}
