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
#' @export
#'
org_teams = function(org, filter=NULL, exclude=FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::safely(github_api_org_teams)(org)

  status_msg(
    res,
    fail = glue::glue("Failed to retrieve teams for org {usethis::ui_value(org)}.")
  )

  if (failed(res) | empty_result(res)) {
    character()
  } else {
    teams = purrr::map_chr(result(res), "name")
    filter_results(teams, filter, exclude)
  }
}


