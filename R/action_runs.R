github_api_action_runs = function(
  repo, actor = NULL, string = NULL, event = NULL, status = NULL
) {
  arg_is_chr_scalar(repo)
  arg_is_chr_scalar(actor, string, event, status, allow_null = TRUE)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/actions/runs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    actor	= actor,
    string = string,
    event = event,
    status = status
  )
}

#' @name action
#' @rdname action
#'
#' @title Return a data frame containing details on a repository workflow runs.
#'
#' @param repo Character. Address of repository in `owner/name` format.
#'
#' @examples
#' \dontrun{
#' action_runs("rundel/ghclass")
#' }
#'
#' @export
#'
action_runs = function(repo) {
  arg_is_chr_scalar(repo)

  res = purrr::safely(github_api_action_runs)(repo)

  status_msg(
    res,
    fail = "Failed to retrieve workflow runs for repo {.val {repo}}."
  )

  if (failed(res)) {
    NULL
  } else if (empty_result(res) || result(res)[["total_count"]] == 0) {
    tibble::tibble(
      name = character(),
      id = integer(),
      branch = character(),
      commit = character(),
      event = character(),
      status = character(),
      result = character(),
      created = lubridate::ymd_hms()
    )
  } else {
    runs = result(res)[["workflow_runs"]]
    workflows = action_workflows(repo)[,c("name", "id")]
    run_df = tibble::tibble(
      workflow_id = purrr::map_int(runs, "workflow_id"),
      branch = purrr::map_chr(runs, "head_branch"),
      commit = purrr::map_chr(runs, "head_sha"),
      event  = purrr::map_chr(runs, "event"),
      status = purrr::map_chr(runs, "status"),
      result = purrr::map_chr(runs, "conclusion"),
      created = purrr::map_chr(runs, "created_at") %>% lubridate::ymd_hms()
    )

    dplyr::full_join(
      workflows,
      run_df,
      by = c("id" = "workflow_id")
    ) %>%
      dplyr::select(-id)
  }
}
