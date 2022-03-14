github_api_action_run_usage = function(repo, run_id) {
  arg_is_chr_scalar(repo, run_id)

  ghclass_api_v3_req(
    endpoint = "GET /repos/{owner}/{repo}/actions/runs/{run_id}/timing",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    run_id = run_id
  )
}

#' @name action
#' @rdname action
#'
#' @export
#'
action_runtime = function(
    repo,
    branch = NULL,
    event = NULL,
    status = NULL,
    created = NULL,
    limit = 1
) {
  d = action_runs(repo = repo, branch = branch, event = event,
                  status = status, created = created, limit = limit)


  get_run_dur = function(repo, run_id) {
    res = purrr::safely(github_api_action_run_usage)(repo, run_id)

    status_msg(
      res,
      fail = "Failed to retrieve workflow runs for repo {.val {repo}}."
    )

    run_dur = result(res)$run_duration_ms
    if (is.null(run_dur))
      run_dur = NA

    run_dur
  }

  dplyr::mutate(
    d,
    run_dur = purrr::map2_dbl(.data$repo, .data$run_id, get_run_dur),
    run_dur = lubridate::duration(.data$run_dur/1000)
  )
}
