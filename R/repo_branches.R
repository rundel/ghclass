github_api_repo_branches = function(repo) {
  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/branches",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
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


