github_api_team_create = function(org, name, privacy) {
  gh::gh(
    "POST /orgs/:org/teams",
    org=org, name=name, privacy=privacy,
    .token = github_get_token()
  )
}

#' Create team(s)
#'
#' `team_create` creates teams in your GitHub organization
#'
#' @param org character, name of the GitHub organization
#' @param team character, listing one or more teams
#' @param prefix Character. Common team name prefix
#' @param suffix Character. Common team name suffix
#' @param privacy character, level of privacy of teams, closed (visible to all
#' members of the organization) or secret (only visible to organization owners
#' and members of a team), default is closed
#'
#' @examples
#' \dontrun{
#' team_create("ghclass",c("team01","team01"))
#' }
#'

#' @aliases create_team
#'
#' @export
#'
team_create = function(org, team,
                       prefix = "", suffix = "",
                       privacy = c("secret","closed")) {
  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr(team)

  team = unique(as.character(team))
  privacy = match.arg(privacy)

  team = paste0(prefix, team, suffix)

  org_teams = org_teams(org)[["team"]]

  new_teams = setdiff(team, org_teams)
  existing_teams = intersect(team, org_teams)

  if (length(existing_teams) > 0)
    usethis::ui_info("Skipping existing teams: {usethis::ui_value(existing_teams)}.")

  purrr::walk(
    new_teams,
    function(team) {
      res = purrr::safely(github_api_team_create)(
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
