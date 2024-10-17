
github_api_repo_activity = function(repo,  ref = NULL, actor = NULL, time_period = NULL, activity_type = NULL) {
  arg_is_chr_scalar(repo, allow_null = FALSE)
  arg_is_chr_scalar(ref, actor, time_period, activity_type, allow_null = TRUE)

  if (!is.null(time_period))
    stopifnot(time_period %in% c("day", "week", "month", "quarter", "year"))

  if (!is.null(activity_type))
    stopifnot(activity_type %in% c("push", "force_push", "branch_creation", "branch_deletion", "pr_merge", "merge_queue_merge"))

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/activity",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = ref,
    actor = actor,
    time_period = time_period,
    activity_type = activity_type
  )
}

#' @rdname repo_details
#'
#' @param time_period  Character. The time period to filter by.
#' Options are  "all time", "day", "week", "month", "quarter", "year".
#'
#' @export
#'

repo_pushes = function(repo, branch = NULL, author = NULL, time_period = c("all time", "day", "week", "month", "quarter", "year"), quiet = FALSE) {

  time_period = match.arg(time_period)

  if (time_period == "all time")
    time_period = NULL

  arg_is_chr(repo)
  arg_is_chr_scalar(branch, author, time_period, allow_null = TRUE)
  arg_is_lgl_scalar(quiet)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_activity)(
        repo, ref = branch, actor = author, time_period = time_period, activity_type = "push"
      )

      if (!quiet) {
        status_msg(
          res,
          fail = "Failed to retrieve activity from {.val {repo}}."
        )
      }

      pushes = result(res)

      if (empty_result(pushes)) {
        tibble::tibble(
          repo  = character(),
          login = character(),
          ref  = character(),
          activity = character(),
          date  = as.POSIXct(character()),
          before = character(),
          after = character()
        )
      } else {
        tibble::tibble(
          repo   = repo,
          login  = purrr::map_chr(pushes, c("actor", "login"), .default = NA),
          ref    = purrr::map_chr(pushes, c("ref"), .default = NA),
          activity = purrr::map_chr(pushes, c("activity_type"), .default = NA),
          date   = lubridate::ymd_hms(
            purrr::map_chr(pushes, c("timestamp"), .default = NA)
          ),
          before = purrr::map_chr(pushes, c("before"), .default = NA),
          after  = purrr::map_chr(pushes, c("after"), .default = NA)
        )
      }
    }
  )
}
