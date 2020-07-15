#' @name pr
#' @rdname pr
#'
#' @title GitHub Pull Request related tools
#'
#' @description
#'
#' * `pr_create` - create a pull request GitHub from the `base` branch to the `head` branch.
#'
#' @param repo Character. Address of one or more repositories in "owner/name" format.
#' @param title Character. Title of the pull request.
#' @param head Character. The name of the branch where your changes are implemented.
#' For cross-repository pull requests in the same network, namespace `head` with a user
#' like this: `username:branch`.
#' @param base Character. The name of the branch you want the changes pulled into.
#' This should be an existing branch on the current repository. You cannot submit
#' a pull request to one repository that requests a merge to a base of another repository.
#' @param body Character. The text contents of the pull request.
#' @param draft Logical. Should the pull request be created as a draft pull request
#' (these cannot be merged until allowed by the author).
#'
#' @seealso [repo_issues]
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test", "test_pr", auto_init=TRUE)
#'
#' branch_create("ghclass-test/test_pr", new_branch = "test")
#'
#' repo_modify_file("ghclass-test/test_pr", "README.md", pattern = "test_pr",
#'                  content = "Hello", method = "after", branch = "test")
#'
#' pr_create("ghclass-test/test_pr", "merge", "test")
#'
#' repo_delete("ghclass-test/test_pr", prompt = FALSE)
#' }

NULL

