github_api_action_workflows = function(repo) {
  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/actions/workflows",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}

#' @name action
#' @rdname action
#'
#' @title Return a data frame containing details on a repository workflows.
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param full Logical. Should all workflow columns be returned
#'
#' @examples
#' \dontrun{
#' action_workflows("rundel/ghclass")
#' }
#'
#' @export
#'
action_workflows = function(repo, full = FALSE) {
  arg_is_chr_scalar(repo)

  res = purrr::safely(github_api_action_workflows)(repo)

  status_msg(
    res,
    fail = "Failed to retrieve workflows for repo {.val {repo}}."
  )

  if (failed(res) | empty_result(res)) {
    d = tibble::tibble(
      id         = integer(),
      node_id    = character(),
      name       = character(),
      path       = character(),
      state      = character(),
      created_at = character(),
      updated_at = character(),
      url        = character(),
      html_url   = character(),
      badge_url  = character()
    )
  } else {
    d = purrr::map_dfr(result(res)[["workflows"]], ~ .x)
  }

  if (!full)
    d = dplyr::select(d, .data$name, .data$path, .data$state, .data$badge_url)

  d
}
