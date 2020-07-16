github_api_org_teams = function(org) {
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/teams", 
    org = org
  )
}

#' @rdname org_details
#' @param team_type Character. Either "slug" if the team names are slugs or "name" if full team names are provided.
#' @export
#'
org_teams = function(org, filter = NULL, exclude = FALSE, team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)
  team_type = match.arg(team_type)

  res = purrr::safely(github_api_org_teams)(org)

  status_msg(
    res,
    fail = "Failed to retrieve teams for org {.val {org}}."
  )

  if (failed(res) | empty_result(res)) {
    character()
  } else {
    teams = purrr::map_chr(result(res), team_type)
    filter_results(teams, filter, exclude)
  }
}


