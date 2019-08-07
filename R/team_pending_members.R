github_api_team_pending_members = function(id) {
  gh::gh(
    "GET /teams/:id/invitations",
    id = id,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' Get pending team members
#'
#' `team_pending_members` returns a data frame of pending team members.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#'
#' @examples
#' \dontrun{
#' team_pending_members("ghclass",c("team01","team02"))
#' }
#'
#' @aliases get_pending_team_members
#'
#' @export
#'
team_pending_members = function(org, team = org_teams(org)) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)

  team = team_id_lookup(team, org)

  purrr::pmap_df(
    team,
    function(team, id) {
      res = purrr::safely(github_api_team_pending_members)(id)

      status_msg(
        res,
        fail = glue::glue("Failed to retrieve team members for {usethis::ui_value(team)}.")
      )

      if (failed(res) | empty_result(res)) {
        tibble::tibble(
          team = character(),
          user = character()
        )
      } else {
        tibble::tibble(
          team = team,
          user = purrr::map_chr(result(res), "login")
        )
      }
    }
  )
}
