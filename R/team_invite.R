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
#' @family github organization team related functions
#'
#' @export
team_invite = function(org, user, team, create_missing_teams = TRUE) {
  stopifnot(!missing(org))

  stopifnot(is.character(user) & length(user) >=1)
  stopifnot(is.character(team) & length(team) >=1)
  stopifnot(length(create_missing_teams) == 1)

  d = tibble::tibble(user, team)

  org_teams = get_teams(org)
  new_teams = setdiff(unique(team), org_teams[["team"]])

  if (length(new_teams) > 0 & create_missing_teams) {
    team_create(org, new_teams)
    org_teams = get_teams(org)
  }

  d = team_id_lookup(d, org_teams)

  purrr::pwalk(
    d,
    function(user, team, id) {
      res = purrr::safely(github_api_team_invite)(id, user)

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(user)} to team {usethis::ui_value(team)}."),
        glue::glue("Failed to add {usethis::ui_value(user)} to team {usethis::ui_value(team)}.")
      )
    }
  )
}
