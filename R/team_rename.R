github_api_team_rename = function(id, new_name) {
  gh::gh(
    "PATCH /teams/:team_id",
    team_id = id,
    name = new_name,
    .token = github_get_token()
  )
}


#' Rename existing team(s)
#'
#' `team_rename` renames an existing team within the given GitHub organization.
#'
#' @param org character, name of the GitHub organization
#' @param team character, one or more existing team names
#' @param new_team character, one or more new team names
#'
#' @examples
#' \dontrun{
#' team_rename("ghclass-test", "hw1-team01", "hw01-team01")
#' }
#'
#' @family github organization team related functions
#'
#' @export
team_rename = function(org, team, new_team) {
  arg_is_chr_scalar(org)

  d = tibble::tibble(
    team = team,
    new_team = new_team
  )

  d = team_id_lookup(d, org_teams(org))

  purrr::pwalk(
    d,
    function(team, id, new_team) {
      res = purrr::safely(github_api_team_rename)(id, new_team)

      status_msg(
        res,
        glue::glue("Renamed team {usethis::ui_value(team)} to {usethis::ui_value(new_team)}."),
        glue::glue("Failed to rename team {usethis::ui_value(team)} to {usethis::ui_value(new_team)}.")
      )
    }
  )
}
