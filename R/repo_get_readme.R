github_api_repo_get_readme = function(repo, branch) {
  arg_is_chr_scalar(repo, branch)

  gh::gh(
    "GET /repos/:owner/:repo/readme",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = branch,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' @rdname repo_file
#'
#' @export
#'
repo_get_readme = function(repo, branch = "master", include_details = TRUE) {
  arg_is_chr_scalar(repo, branch)

  file = purrr::possibly(github_api_repo_get_readme, NULL)(repo, branch)
  extract_content(repo, path = "README.md", file = file, include_details = include_details)
}
