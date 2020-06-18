github_api_team_members = function(id) {
  gh::gh(
    "GET /teams/:id/members",
    id = id,
    role = "all",
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}


#' Get team members
#'
#' `team_members` returns a data frame of teams and their members.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#'
#' @examples
#' \dontrun{
#' team_members("ghclass-test",c("team01","team02"))
#' }
#'
#' @export
#'
team_members = function(org, team = org_teams(org)) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)

  team = team_id_lookup(team, org)

  purrr::pmap_df(
    team,
    function(team, id) {
      res = purrr::safely(github_api_team_members)(id)

      status_msg(
        res,
        fail = "Failed to retrieve team members for {.val {team}}."
      )

      if (failed(res) | empty_result(res)) {
        tibble::tibble(
          team = character(),
          user = character()
        )
      } else {
        tibble::tibble(
          team = team,
          user = purrr::map_chr(result(res), "login"),
        )
      }
    }
  )
}
