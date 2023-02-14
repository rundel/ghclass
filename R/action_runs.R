github_api_action_runs = function(
  repo, actor = NULL, branch = NULL, event = NULL, status = NULL,
  created = NULL,
  limit = 1
) {
  arg_is_chr_scalar(repo)
  arg_is_chr_scalar(actor, branch, event, status, allow_null = TRUE)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/actions/runs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    actor	= actor,
    branch = branch,
    event = event,
    status = status,
    created = created,
    limit = limit
  )
}

github_api_action_workflow_runs = function(
    repo, workflow_id,
    actor = NULL, branch = NULL, event = NULL,
    status = NULL, created = NULL,
    limit = 1
) {
  arg_is_chr_scalar(repo)
  arg_is_pos_int_scalar(workflow_id)
  arg_is_chr_scalar(actor, branch, event, status, allow_null = TRUE)

  ghclass_api_v3_req(
    endpoint = "GET /repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    workflow_id = workflow_id,
    actor	= actor,
    branch = branch,
    event = event,
    status = status,
    created = created,
    limit = limit
  )
}


#' @name action
#' @rdname action
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param branch Character. Filter runs associated with a particular branch.
#' @param event Character. Filter runs for triggered by a specific event. See
#' [here](https://docs.github.com/en/actions/automating-your-workflow-with-github-actions/events-that-trigger-workflows)
#' for possible event names.
#' @param status Character. Filter runs for a particular status or conclusion (e.g. `completed` or `success`).
#' @param created Character. Filter runs for a given creation date.
#' See [here](https://docs.github.com/en/search-github/getting-started-with-searching-on-github/understanding-the-search-syntax#query-for-dates)
#' for date query syntax.
#' @param limit Numeric. Maximum number of workflow runs to return. Default `1`. Note results
#' are chronologically ordered, so `limit = 1` will return the most recent action run for a repository.
#'
#' @export
#'
action_runs = function(
  repo,
  branch = NULL,
  event = NULL,
  status = NULL,
  created = NULL,
  limit = 1
) {
  arg_is_chr(repo)
  arg_is_chr_scalar(branch, event, status, created, allow_null = TRUE)
  arg_is_pos_int_scalar(limit)

  purrr::map_dfr(
    repo,
    function(repo) {
      purrr::pmap_dfr(
        action_workflows(repo)[,c("name", "id")],
        function(name, id) {
          res = purrr::safely(github_api_action_workflow_runs)(
            repo, workflow_id = id,
            branch = branch,
            event = event, status = status,
            created = created, limit = limit
          )

          status_msg(
            res,
            fail = "Failed to retrieve workflow runs for repo {.val {repo}}."
          )

          if (failed(res) || empty_result(res) || result(res)[["total_count"]] == 0) {
            tibble::tibble(
              repo = character(),
              workflow = character(),
              run_id = character(),
              branch = character(),
              commit = character(),
              actor = character(),
              event = character(),
              status = character(),
              conclusion = character(),
              created = lubridate::ymd_hms()
            )
          } else {
            runs = result(res)[["workflow_runs"]]
            run_df = tibble::tibble(
              repo   = repo,
              workflow = name,
              run_id = purrr::map_chr(runs, "id", .default = NA),
              branch = purrr::map_chr(runs, "head_branch", .default = NA),
              commit = purrr::map_chr(runs, "head_sha", .default = NA),
              actor  = purrr::map_chr(runs, c("actor", "login"), .default = NA),
              event  = purrr::map_chr(runs, "event", .default = NA),
              status = purrr::map_chr(runs, "status", .default = NA),
              conclusion = purrr::map_chr(runs, "conclusion", .default = NA),
              created = purrr::map_chr(runs, "created_at", .default = NA) %>% lubridate::ymd_hms()
            )
          }
        }
      )
    }
  )
}


#' @name action
#' @rdname action
#'
#' @export
#'
action_status = function(
    repo,
    branch = NULL,
    event = NULL,
    status = NULL,
    created = NULL,
    limit = 1
) {
  lifecycle::deprecate_warn("0.2.2", "action_status()", "action_runs()")

  action_runs(repo = repo, branch = branch, event = event,
              status = status, created = created, limit = limit)
}
