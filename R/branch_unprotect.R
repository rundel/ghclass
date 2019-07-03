github_api_unprotect_branch = function(repo, branch) {
  gh::gh(
    "DELETE /repos/:owner/:repo/branches/:branch/protection",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    branch = branch,
    .token = github_get_token()
  )
}


#' Unprotect branch
#'
#' `unprotect_branch` removes protections from the specified branch. See
#' <https://help.github.com/en/articles/about-protected-branches> for more details
#' on what this changes.
#'
#' @param repo github repository address in `owner/repo` format
#' @param branch name of the branch to unprotect
#'
#' @family branch functions
#'
#' @export
#'
unprotect_branch = function(repo, branch = "master") {
  arg_is_chr(repo, branch)

  purrr::walk2(
    repo, branch,
    function(repo, branch) {

      res = purrr::safely(github_api_unprotect_branch)(repo, branch)

      repo_fmt = usethis::ui_value(format_repo(repo, branch))

      status_msg(
        res,
        glue::glue("Removing protection from branch {repo_fmt}."),
        glue::glue("Failed to remove protection from branch {repo_fmt}.")
      )
    }
  )
}
