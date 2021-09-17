github_api_issue_edit = function(
  repo, number,
  title = NULL, body = NULL, state = NULL, milestone = NULL,
  labels = list(), assignees = list()
) {
  ghclass_api_v3_req(
    endpoint  = "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner     = get_repo_owner(repo),
    repo      = get_repo_name(repo),
    issue_number = number,
    title     = title,
    body      = body,
    assignee  = NULL,
    state     = state,
    milestone = milestone,
    labels    = I(labels),
    assignees = I(assignees)
  )
}

#' @rdname issue
#'
#' @param state	Character. State of the issue. Either "open" or "closed".
#' @param milestone Character. The number of the milestone to associate this issue with.
#' Only users with push access can set the milestone for issues. The milestone is silently dropped otherwise.
#'
#' @export
#'
issue_edit = function(
  repo, number,
  title = NULL, body = NULL,
  state = NULL, milestone = NULL,
  labels = list(), assignees = list()
) {

  arg_is_chr_scalar(repo)
  arg_is_pos_int_scalar(number)
  arg_is_chr_scalar(title, body, state, milestone, allow_null = TRUE)

  if (!is.list(labels))
    labels = list(labels)

  if (!is.list(assignees))
    assignees = list(assignees)

  res = purrr::safely(github_api_issue_edit)(
    repo = repo, number = number, title = title, body = body,
    state = state, milestone = milestone, labels = labels, assignees = assignees
  )

  status_msg(
    res,
    "Edited issue #{.val {number}} for repo {.val {repo}}.",
    "Failed to edit issue #{.val {number}} for repo {.val {repo}}."
  )

  invisible(res)
}
