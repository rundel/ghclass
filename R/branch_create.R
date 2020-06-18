github_api_branch_get_ref = function(repo, branch="master") {
  gh::gh(
    "GET /repos/:owner/:repo/commits/:ref",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("heads/", branch),
    .token = github_get_token()
  )

}

get_branch_ref = function(repo, branch) {
  arg_is_chr_scalar(repo, branch)

  res = purrr::safely(github_api_branch_get_ref)(repo, branch)
  repo_txt = format_repo(repo, branch)
  if (failed(res))
    cli_stop("Unable to locate branch {.val {repo_txt}}.")

  result(res)
}

github_api_branch_create = function(repo, cur_branch, new_branch) {
  head = get_branch_ref(repo, cur_branch)

  gh::gh(
    "POST /repos/:owner/:repo/git/refs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("refs/heads/",new_branch),
    sha = head[["sha"]],
    .token = github_get_token()
  )
}




#' Create branch
#'
#' `branch_create` creates a new branch from an existing git repo.
#'
#' @param repo github repository address in `owner/repo` format
#' @param cur_branch name of existing branch
#' @param new_branch name of branch to create
#'
#' @export
#'
branch_create = function(repo, cur_branch = "master", new_branch) {
  arg_is_chr(repo, cur_branch, new_branch)

  purrr::pwalk(
    list(repo, cur_branch, new_branch),
    function(repo, cur_branch, new_branch) {

      cur_repo = format_repo(repo, cur_branch)
      new_repo = format_repo(repo, new_branch)

      branches = repo_branches(repo)

      if (!cur_branch %in% branches) {
        cli::cli_alert_danger("Failed to create branch, {.val {cur_repo}} does not exist.")
        return(NULL)
      }

      if (new_branch %in% branches) {
        cli::cli_alert_danger("Skipping creation of branch {.val {new_repo}}, it already exists.")
        return(NULL)
      }

      res = purrr::safely(github_api_branch_create)(repo, cur_branch, new_branch)

      status_msg(
        res,
        "Created branch {.val {new_branch}} from {.val {cur_repo}}.",
        "Failed to create branch {.val {new_branch}} from {.val {cur_repo}}."
      )
    }
  )
}
