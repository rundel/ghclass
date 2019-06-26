github_api_get_branch_ref = function(repo, branch="master") {
  gh::gh(
    "GET /repos/:owner/:repo/commits/:ref",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("heads/", branch),
    .token=get_github_token()
  )

}

get_branch_ref = function(repo, branch) {
  arg_is_chr_scalar(repo, branch)

  res = purrr::safely(github_api_get_branch_ref)(repo, branch)

  if (failed(res)) {
    usethis::ui_stop("Unable to locate branch {usethis::ui_value(format_repo(repo, branch))}.")
  }
  result(res)
}

github_api_create_branch = function(repo, cur_branch, new_branch) {
  head = get_branch_ref(repo, cur_branch)

  gh("POST /repos/:owner/:repo/git/refs",
     owner = get_repo_owner(repo),
     repo = get_repo_name(repo),
     ref = paste0("refs/heads/",new_branch),
     sha = head[["sha"]],
     .token=get_github_token())
}




#' Create branch
#'
#' \code{create_branch} creates a new branch from an existing git repo.
#'
#' @param repo github repository address in `owner/repo` format
#' @param cur_branch name of existing branch
#' @param new_branch name of branch to create
#'
#' @family branch functions
#'
#' @export
#'
create_branch = function(repo, cur_branch = "master", new_branch) {
  arg_is_chr(repo, cur_branch, new_branch)

  purrr::pwalk(
    list(repo, cur_branch, new_branch),
    function(repo, cur_branch, new_branch) {
      res = purrr::safely(github_api_create_branch)(repo, cur_branch, new_branch)

      repo_fmt = usethis::ui_value(format_repo(repo, cur_branch))

      status_msg(
        res,
        glue::glue("Created branch {usethis::ui_value(new_branch)} from {repo_fmt}."),
        glue::glue("Failed to create branch {usethis::ui_value(new_branch)} from {repo_fmt}.")
      )
    }
  )
}


github_api_protect_branch = function(repo, branch) {
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
    .token = get_github_token()
  )
}


#' Protect branch
#'
#' `protect_branch`` turns on protection for the specified branch. See
#' [https://help.github.com/en/articles/about-protected-branches] for more details
#' on what this changes.
#'
#' @param repo github repository address in `owner/repo` format
#' @param branch name of the branch to protect
#'
#' @family branch functions
#'
#' @export
#'
protect_branch = function(repo, branch = "master") {
  arg_is_chr(repo, branch)

  purrr::walk2(
    repo, branch,
    function(repo, branch) {
      res = purrr::safely(github_api_protect_branch)(repo, branch)

      repo_fmt = usethis::ui_value(format_repo(repo, branch))

      status_msg(
        res,
        glue::glue("Protecting branch {repo_fmt}."),
        glue::glue("Failed to protect branch {repo_fmt}.")
      )
    }
  )
}


github_api_unprotect_branch = function(repo, branch) {
  gh::gh(
    "DELETE /repos/:owner/:repo/branches/:branch/protection",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    branch = branch,
    .token = get_github_token()
  )
}


#' Unprotect branch
#'
#' \code{unprotect_branch} removes protections from the specified branch. See
#' [https://help.github.com/en/articles/about-protected-branches] for more details
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
