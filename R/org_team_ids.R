#' Get organization teams and their ids
#'
#' `org_team_ids` returns a (filtered) data frame of teams in the organization with their
#' names (`name`) and unique ids (`id`).
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
org_team_ids = function(org, filter=NULL, exclude=FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::safely(github_api_org_teams)(org)

  status_msg(
    res,
    fail = "Failed to retrieve teams for org {.val {org}}."
  )

  if (failed(res) | empty_result(res)) {
    tibble::tibble(
      team = character(),
      id   =  integer()
    )
  } else {
    d = purrr::map_dfr(
      result(res),
      ~ list(team = .x[["name"]], id = .x[["id"]])
    )
    filter_results(d, "team", filter, exclude)
  }
}
