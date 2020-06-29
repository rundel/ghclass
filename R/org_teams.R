github_api_org_teams = function(org) {
  gh::gh(
    "GET /orgs/:org/teams", org=org,
    .token = github_get_token(),
    .limit=github_get_api_limit()
  )
}

#' @rdname org
#' @export
#'
org_teams = function(org, filter=NULL, exclude=FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::safely(github_api_org_teams)(org)

  status_msg(
    res,
    fail = "Failed to retrieve teams for org {.val {org}}."
  )

  if (failed(res) | empty_result(res)) {
    character()
  } else {
    teams = purrr::map_chr(result(res), "name")
    filter_results(teams, filter, exclude)
  }
}


