github_api_team_repos = function(team_id) {
  gh::gh(
    "GET /teams/:id/repos",
    id=team_id,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' Get teams' repos
#'
#' `team_repos` returns a (filtered) data frame of teams and their repos.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#'
#' @examples
#' \dontrun{
#' team_repos("ghclass",c("team01","team02"))
#' }
#'

#' @aliases get_team_repos
#'
#' @export
#'
team_repos = function(org, team) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)

  team = team_id_lookup(team, org)

  purrr::pmap_dfr(
    team,
    function(team, id) {
      res = purrr::safely(github_api_team_repos)(id)

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
