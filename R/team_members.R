github_api_team_members = function(id) {
  gh::gh(
    "GET /teams/:id/members",
    id = id,
    role = "all",
    .token = github_get_token(),
    .limit = get_github_api_limit()
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
#' @family github organization team related functions
#'
#' @export
#'
team_members = function(org, team = org_teams(org)) {
  arg_is_chr_scalar(org)

  if (is.character(team))
    team = get_specific_teams(org, team)

  stopifnot(all(c("team","id") %in% names(team)))

  purrr::pmap_df(
    team,
    function(team, id) {
      res = purrr::safely(github_api_team_members)(id)

      status_msg(
        res,
        fail = glue::glue("Failed to retrieve team members for {usethis::ui_value(team)}.")
      )

      if (empty_result(res)) {
        tibble::tibble(
          team = character(),
          github = character()
        )
      } else {
        tibble::tibble(
          team = team,
          github = purrr::map_chr(result(res), "login"),
        )
      }
    }
  )
}
