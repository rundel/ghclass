github_api_get_team_repos = function(team_id) {
  gh::gh(
    "GET /teams/:id/repos",
    id=team_id,
    .token = github_get_token(),
    .limit = get_github_api_limit()
  )
}

#' Get teams' repos
#'
#' `get_team_repos` returns a (filtered) data frame of teams and their repos.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#'
#' @examples
#' \dontrun{
#' get_team_repos("ghclass",c("team01","team02"))
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_team_repos = function(org, team) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)

  team = get_specific_teams(org, team)

  purrr::pmap_dfr(
    team,
    function(team, id) {
      res = purrr::safely(github_api_get_team_repos)(id)

      if (succeeded(res) & !empty_result(result(res))) {
        tibble::tibble(
          team = team,
          repo = purrr::map_chr(result(res), "full_name")
        )
      } else {
        tibble::tibble(
          team = character(),
          repo = character()
        )
      }
    }
  )
}
