github_api_action_artifacts = function(repo) {
  arg_is_chr_scalar(repo)

  ghclass_api_v3_req(
    endpoint = "GET /repos/{owner}/{repo}/actions/artifacts",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}

#' @name action
#' @rdname action
#'
#' @param repo Character. Address of repositories in `owner/name` format.
#' @param keep_expired Logical. Should expired artifacts be returned.
#' @param which Character. Either `"latest"` to return only the most recent of each
#'   artifact or `"all"` to return all artifacts.
#' @param filter_branch Character. Regex pattern to filter artifacts by branch name.
#' @param exclude_branch Logical. If `TRUE`, exclude branches matching `filter_branch` instead of including them.
#' @param filter Character. Regex pattern to filter artifacts by artifact name.
#' @param exclude Logical. If `TRUE`, exclude artifact names matching `filter` instead of including them.
#' @export
#'
action_artifacts = function(repo, filter = NULL, exclude = FALSE,
                            keep_expired = FALSE, which = c("latest", "all"),
                            filter_branch = NULL, exclude_branch = FALSE) {
  which = match.arg(which)

  arg_is_chr(repo)
  arg_is_chr_scalar(which)
  arg_is_lgl_scalar(keep_expired, exclude_branch, exclude)
  arg_is_chr_scalar(filter_branch, allow_null = TRUE)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::map_dfr(
    repo,
    function(r) {
      res = purrr::safely(github_api_action_artifacts)(r)
      status_msg(
        res,
        fail = "Failed to retrieve artifacts for repo {.val {repo}}."
      )

      if (failed(res) || empty_result(res) || result(res)[["total_count"]] == 0) {
        tibble::tibble(
          repo    = character(),
          branch  = character(),
          name    = character(),
          id      = integer(),
          size    = integer(),
          url     = character(),
          expired = logical(),
          created = lubridate::ymd_hms(),
          updated = lubridate::ymd_hms(),
          expires = lubridate::ymd_hms()
        )
      } else {
        artifacts = result(res)[["artifacts"]]

        tibble::tibble(
          repo    = r,
          branch  = purrr::map_chr(artifacts, c("workflow_run", "head_branch"), .default = NA),
          name    = purrr::map_chr(artifacts, "name", .default = NA),
          id      = purrr::map_dbl(artifacts, "id", .default = NA),
          size    = purrr::map_dbl(artifacts, "size_in_bytes", .default = NA),
          url     = purrr::map_chr(artifacts, "url", .default = NA),
          expired = purrr::map_lgl(artifacts, "expired", .default = NA),
          created = purrr::map_chr(artifacts, "created_at", .default = NA) %>% lubridate::ymd_hms(),
          updated = purrr::map_chr(artifacts, "updated_at", .default = NA) %>% lubridate::ymd_hms(),
          expires = purrr::map_chr(artifacts, "expires_at", .default = NA) %>% lubridate::ymd_hms()
        )
      }
    }
  )

  if (!keep_expired) {
    res = dplyr::filter(res, .data[["expired"]] == FALSE)
  }

  res = filter_results(res, filter_branch, exclude_branch, col = "branch")
  res = filter_results(res, filter,        exclude,        col = "name")

  if (which == "latest") {
    res %>%
      dplyr::group_by(.data[["repo"]], .data[["name"]]) %>%
      dplyr::arrange(dplyr::desc(.data[["created"]])) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else if (which == "all") {
    res
  } else {
    cli_stop("Invalid which choice.")
  }

}
