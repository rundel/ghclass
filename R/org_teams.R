github_api_org_teams = function(org) {
  gh::gh(
    "GET /orgs/:org/teams", org=org,
    .token = github_get_token(),
    .limit=get_github_api_limit()
  )
}

#' Get organization teams
#'
#' `org_teams` returns a (filtered) data frame of teams in the organization with columns for
#' their names (`name`) and their unique ids (`id`).
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'

#' @aliases get_teams
#'
#' @export
#'
org_teams = function(org, filter=NULL, exclude=FALSE) {
  arg_is_chr_scalar(org)
  arg_is_lgl_scalar(exclude)
  stopifnot(length(filter)<=1)

  res = github_api_org_teams(org)

  teams = if (empty_result(res)) {
    tibble::tibble(
      team = character(),
      id   =  integer()
    )
  } else {
    tibble::tibble(
      team = purrr::map_chr(res, "name"),
      id   = purrr::map_int(res, "id")
    )
  }

  filter_results(teams, "team", filter, exclude)
}
