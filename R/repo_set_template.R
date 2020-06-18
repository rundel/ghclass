#' Change the template status of a GitHub repository
#'
#' `repo_set_template` returns TRUE if the github repository is a template repository.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param status Logical. Should the repository be set as a template repository
#'
#' @export
#'
repo_set_template = function(repo, status = TRUE) {

  arg_is_chr(repo)
  arg_is_lgl(status)

  # Checking if repo exists
  purrr::walk2(
    repo, status,
    function(repo, status) {
      res = purrr::safely(github_api_repo_edit)(repo, is_template = status)
      status_msg(
        res,
        "Changed the template status of repo {.val {repo}} to {.val {status}}.",
        "Failed to change template status of repo {.val {repo}}."
      )
    }
  )
}
