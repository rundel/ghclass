github_api_repo_branches = function(repo) {
  gh::gh(
    "GET /repos/:owner/:repo/branches",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token(),
    .limit=github_get_api_limit()
  )
}

#' @rdname repo_details
#' @export
#'
repo_branches = function(repo) {
  arg_is_chr_scalar(repo)

  res = purrr::safely(github_api_repo_branches)(repo)

  status_msg(
    res,
    fail = "Failed to retrieve branches for repo {.val {repo}}."
  )

  if (failed(res) | empty_result(res)) {
    character()
  } else {
    teams = purrr::map_chr(result(res), "name")
  }
}


