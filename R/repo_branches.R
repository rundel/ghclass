github_api_repo_branches = function(repo) {
  gh::gh(
    "GET /repos/:owner/:repo/branches",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token(),
    .limit=github_get_api_limit()
  )
}

#' Get repository branches
#'
#' `repo_branches` returns a (filtered) vector of branches for the current repository.
#'
#' @param repo character, a single GitHub repository address in `owner/repo` format
#' @param filter character, a regex pattern for matching (or excluding) branches.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @export
#'
repo_branches = function(repo, filter=NULL, exclude=FALSE) {
  arg_is_chr_scalar(repo)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::safely(github_api_repo_branches)(repo)

  status_msg(
    res,
    fail = glue::glue("Failed to retrieve branches for repo {.val {repo}}.")
  )

  if (failed(res) | empty_result(res)) {
    character()
  } else {
    teams = purrr::map_chr(result(res), "name")
    filter_results(teams, filter, exclude)
  }
}


