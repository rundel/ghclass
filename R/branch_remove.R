github_api_branch_remove = function(repo, branch) {
  gh::gh(
    "DELETE /repos/:owner/:repo/git/refs/:ref",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("heads/", branch),
    .token = github_get_token()
  )
}


#' Remove branch
#'
#' `branch_remove` deletes a branch from an existing GitHub repository.
#'
#' @param repo github repository address in `owner/repo` format
#' @param branch name of existing branch
#'
#' @export
#'
branch_remove = function(repo, branch) {
  arg_is_chr(repo, branch)

  purrr::pwalk(
    list(repo, branch),
    function(repo, branch) {
      res = purrr::safely(github_api_branch_remove)(repo, branch)

      status_msg(
        res,
        glue::glue("Removed branch {usethis::ui_value(format_repo(repo, branch))}."),
        glue::glue("Failed to remove branch {usethis::ui_value(format_repo(repo, branch))}.")
      )
    }
  )
}
