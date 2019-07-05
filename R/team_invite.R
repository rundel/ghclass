github_api_team_invite = function(team_id, username) {
  gh(
    "PUT /teams/:id/memberships/:username",
    id=team_id, username=username, role="member",
    .token = github_get_token()
  )
}

#' Add Members to an Organizaton's Team(s)
#'
#' `team_invite` add members to GitHub Organization Teams.
#'
#' @param org character, name of the GitHub organization
#' @param user character, one or more usernames to invite
#' @param team character, one or more team names
#' @param create_missing_teams logical, if a team does not already exist
#' should it be added to the GitHub organization.
#'
#' @examples
#' \dontrun{
#' team_invite("ghclass-test", "rundel", c("hw1-team01","hw1-team02"))
#' }
#'

#' @aliases add_team_member
#'
#' @export
team_invite = function(org, user, team, create_missing_teams = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(user, team)
  arg_is_lgl_scalar(create_missing_teams)

  d = tibble::tibble(
    team = team,
    user = user
  )

  teams = team_id_lookup(d, org)

  new_teams = teams[["team"]][ is.na(teams[["id"]]) ]

  if (length(new_teams) > 0 & create_missing_teams) {
    team_create(org, new_teams)
    teams = team_id_lookup(d, org)
  }

  purrr::pwalk(
    teams,
    function(team, id, user) {
      if (missing_team(id, org)) return()

      res = purrr::safely(github_api_team_invite)(id, user)

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(user)} to team {usethis::ui_value(team)}."),
        glue::glue("Failed to add {usethis::ui_value(user)} to team {usethis::ui_value(team)}.")
      )
    }
  )
}
