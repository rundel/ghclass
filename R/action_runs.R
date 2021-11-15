github_api_action_runs = function(
  repo, actor = NULL, string = NULL, event = NULL, status = NULL, limit = 30
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
    status = status,
    limit = limit
  )
}

#' @name action
#' @rdname action
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param limit Numeric. Maximum number of workflow runs to return. Defualt `50`.
#'
#' @export
#'
action_runs = function(repo, limit = 50) {
  arg_is_chr_scalar(repo)
  arg_is_pos_int_scalar(limit)

  res = purrr::safely(github_api_action_runs)(repo, limit = limit)

  status_msg(
    res,
    fail = "Failed to retrieve workflow runs for repo {.val {repo}}."
  )

  if (failed(res)) {
    NULL
  } else if (empty_result(res) || result(res)[["total_count"]] == 0) {
    tibble::tibble(
      name = character(),
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
      workflow_id = purrr::map_int(runs, "workflow_id", .default = NA),
      branch = purrr::map_chr(runs, "head_branch", .default = NA),
      commit = purrr::map_chr(runs, "head_sha", .default = NA),
      event  = purrr::map_chr(runs, "event", .default = NA),
      status = purrr::map_chr(runs, "status", .default = NA),
      result = purrr::map_chr(runs, "conclusion", .default = NA),
      created = purrr::map_chr(runs, "created_at", .default = NA) %>% lubridate::ymd_hms()
    )

    dplyr::full_join(
      workflows,
      run_df,
      by = c("id" = "workflow_id")
    ) %>%
      dplyr::select(-"id")
  }
}
