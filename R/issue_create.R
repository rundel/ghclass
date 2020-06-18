github_api_issue_create = function(repo, title, body, labels, assignees){
  gh::gh(
    "POST /repos/:owner/:repo/issues",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    title = title,
    body = body,
    labels = I(labels),
    assignees = I(assignees),
    .token = github_get_token()
  )
}

#' Create an issue
#'
#' `issue_create` creates an issue for a GitHub repository.
#'
#' @param repo Character. Address of one or more repositories in `owner/name` format.
#' @param title Character. Title of the issue.
#' @param body Character. Content of the issue.
#' @param labels Character. Vector of the labels to associate with this issue
#' @param assignees Character. Vector of logins for users assigned to the issue.
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test","issue-test")
#'
#' issue_create("ghclass-test/issue-test", title = "Issue 1", body = "This is an issue")
#'
#' issue_create(
#'   "ghclass-test/issue-test",
#'   title = "Issue 2", body = "This is also issue",
#'   label = "Important"
#' )
#'
#' issue_create(
#'   "ghclass-test/issue-test",
#'   title = "Issue 3", body = "This is also issue",
#'   label = c("Important", "Super Important")
#'   assignees = "rundel"
#' )
#' }
#'
#' @export
#'

issue_create = function(repo, title, body, labels = character(), assignees = character()) {

  arg_is_chr(repo, title, body)

  if (!is.list(labels))
    labels = list(labels)

  if (!is.list(assignees))
    assignees = list(assignees)

  res = purrr::pmap(
    list(repo, title, body, labels, assignees),
    function(repo, title, body, labels, assignees) {
      res = purrr::safely(github_api_issue_create)(
        repo, title, body, labels, assignees
      )

      status_msg(
        res,
        glue::glue("Created issue {.val {title}} for repo {.val {repo}}."),
        glue::glue("Failed to create issue {.val {title}} for repo {.val {repo}}.")
      )

      res
    }
  )

  invisible(res)
}
