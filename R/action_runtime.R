github_api_action_run_usage = function(repo, run_id) {
  arg_is_chr_scalar(repo)
  arg_is_pos_int_scalar(run_id)

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
      fail = "Failed to retrieve run time for run {.val {run_id}} from repo {.val {repo}}."
    )

    run_dur = result(res)$run_duration_ms
    if (is.null(run_dur))
      run_dur = NA

    run_dur
  }

  run_dur = status_scope(
    "Retrieving run times", nrow(d),
    done = "Retrieved run times for {n_ok} of {total} run{?s}",
    purrr::map2_dbl(d[["repo"]], d[["run_id"]], get_run_dur)
  )

  d[["run_dur"]] = lubridate::duration(run_dur / 1000)

  d
}
