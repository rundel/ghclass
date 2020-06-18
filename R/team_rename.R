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
#' @export
team_rename = function(org, team, new_team) {
  arg_is_chr_scalar(org)

  d = tibble::tibble(
    team = team,
    new_team = new_team
  )

  d = team_id_lookup(d, org)

  purrr::pwalk(
    d,
    function(team, id, new_team) {

      if (missing_team(team, id, org)) return()

      res = purrr::safely(github_api_team_rename)(id, new_team)

      status_msg(
        res,
        glue::glue("Renamed team {.val {team}} to {.val {new_team}}."),
        glue::glue("Failed to rename team {.val {team}} to {.val {new_team}}.")
      )
    }
  )
}
