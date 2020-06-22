#' @name branch
#' @rdname branch
#'
#' @title Repository branch tools
#'
#' `branch_create` creates a new branch from an existing GitHub repo.
#'
#' `branch_remove` deletes a branch from an existing GitHub repo.
#'
# #' `branch_protect` / `branch_unprotect` either add or remove protections from the specified branch. See
# #' <https://help.github.com/en/articles/about-protected-branches> for more details
# #' on what this changes.
#'
#' @param repo Github repository address in `owner/repo` format.
#' @param cur_branch name of existing branch
#' @param new_branch name of branch to create
#' @param branch Repository branch to use.
#'
#' @seealso [repo_branches]
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-demo", "test_branch", auto_init=TRUE)
#'
#' branch_create("ghclass-demo/test_branch", new_branch = "test")
#'
#' branch_delete("ghclass-demo/test_branch", new_branch = "test")
#'
#' repo_delete("ghclass-demo/test_branch", prompt = FALSE)
#' }
#'
NULL

