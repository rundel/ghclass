github_api_branch_protect = function(repo, branch) {
  gh::gh(
    "PUT /repos/:owner/:repo/branches/:branch/protection",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    branch = branch,
    required_status_checks = NA,
    enforce_admins = NA,
    required_pull_request_reviews = NA,
    restrictions = list(
      users = list(),
      teams = list()
    ),
    .token = github_get_token()
  )
}


#' Protect branch
#'
#' `branch_protect`` turns on protection for the specified branch. See
#' <https://help.github.com/en/articles/about-protected-branches> for more details
#' on what this changes.
#'
#' @param repo github repository address in `owner/repo` format
#' @param branch name of the branch to protect
#'
#' @family branch functions
#'
#' @aliases protect_branch
#'
#' @export
#'
branch_protect = function(repo, branch = "master") {
  arg_is_chr(repo, branch)

  purrr::walk2(
    repo, branch,
    function(repo, branch) {
      res = purrr::safely(github_api_branch_protect)(repo, branch)

      repo_fmt = usethis::ui_value(format_repo(repo, branch))

      status_msg(
        res,
        glue::glue("Protecting branch {repo_fmt}."),
        glue::glue("Failed to protect branch {repo_fmt}.")
      )
    }
  )
}
