#' @name branch
#' @rdname branch
#'
#' @title Create and delete branches in a repository
#'
#' @description
#' * `branch_create` - creates a new branch from an existing GitHub repo.
#'
#' * `branch_delete` - deletes a branch from an existing GitHub repo.
#'
#' * `branch_remove` - previous name of `branch_delete`, deprecated.
#'
# #' * `branch_protect` / `branch_unprotect` either add or remove protections from the specified branch. See
# #' <https://help.github.com/en/articles/about-protected-branches> for more details
# #' on what this changes.
#'
#' @param repo Github repository address in `owner/repo` format.
#' @param branch Repository branch to use.
#' @param new_branch Name of branch to create.
#'
#' @seealso [repo_branches]
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test", "test_branch", auto_init=TRUE)
#'
#' branch_create("ghclass-test/test_branch", new_branch = "test")
#' repo_branches("ghclass-test/test_branch")
#'
#' branch_delete("ghclass-test/test_branch", branch="test")
#' repo_branches("ghclass-test/test_branch")
#'
#' repo_delete("ghclass-test/test_branch", prompt = FALSE)
#' }
#'
NULL

