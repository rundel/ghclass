github_api_branch_get_ref = function(repo, branch="master") {
  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/commits/:ref",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("heads/", branch)
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

github_api_branch_create = function(repo, branch, new_branch) {
  head = get_branch_ref(repo, branch)

  ghclass_api_v3_req(
    "POST /repos/:owner/:repo/git/refs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("refs/heads/", new_branch),
    sha = head[["sha"]]
  )
}





#' @rdname branch
#' @export
#'
branch_create = function(repo, branch = "master", new_branch) {
  arg_is_chr(repo, branch, new_branch)

  purrr::pwalk(
    list(repo, branch, new_branch),
    function(repo, branch, new_branch) {

      cur_repo = format_repo(repo, branch)
      new_repo = format_repo(repo, new_branch)

      branches = repo_branches(repo)

      if (!branch %in% branches) {
        cli::cli_alert_danger("Failed to create branch, {.val {cur_repo}} does not exist.")
        return()
      }

      if (new_branch %in% branches) {
        cli::cli_alert_danger("Skipping creation of branch {.val {new_repo}}, it already exists.")
        return()
      }

      res = purrr::safely(github_api_branch_create)(repo, branch, new_branch)

      status_msg(
        res,
        "Created branch {.val {new_branch}} in repo {.val {repo}}.",
        "Failed to create branch {.val {new_branch}} in repo {.val {repo}}."
      )
    }
  )
}
