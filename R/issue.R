#' @name issue
#' @rdname issue
#'
#' @title GitHub Issue related tools
#'
#' @description
#' * `issue_create` creates a new issue.
#'
#' * `issue_close` closes an existing issue.
#'
#' * `issue_edit` edits the properties of an existing issue.
#'
#' @param repo Character. Address of one or more repositories in `owner/name` format.
#' @param title Character. Title of the issue.
#' @param body Character. Content of the issue.
#' @param labels Character. Vector of the labels to associate with this issue
#' @param assignees Character. Vector of logins for users assigned to the issue.
#' @param number Integer. GitHub issue number.
#'
#' @return All functions invisibly return a list containing the results of the relevant GitHub API call.
#'
#' @seealso [repo_issues]
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test","test_issue")
#'
#' issue_create(
#'   "ghclass-test/test_issue",
#'   title = "Issue 1",
#'   body = "This is an issue"
#'  )
#'
#' issue_create(
#'   "ghclass-test/test_issue",
#'   title = "Issue 2", body = "This is also issue",
#'   label = "Important"
#' )
#'
#' issue_create(
#'   "ghclass-test/test_issue",
#'   title = "Issue 3", body = "This is also issue",
#'   label = c("Important", "Super Important"),
#'   assignees = "rundel"
#' )
#'
#' issue_close("ghclass-test/test_issue", 1)
#'
#' issue_edit(
#'   "ghclass-test/test_issue", 2,
#'   title = "New issue 2 title!",
#'   body = "Replacement body text"
#' )
#'
#' ghclass::repo_issues("ghclass-test/test_issue")
#'
#' repo_delete("ghclass-test/test_issue", prompt=FALSE)
#' }


NULL

