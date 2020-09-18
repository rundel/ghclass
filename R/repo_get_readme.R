github_api_repo_get_readme = function(repo, branch = NULL) {
  arg_is_chr_scalar(repo)
  arg_is_chr_scalar(branch, allow_null=TRUE)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/readme",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = branch
  )
}

#' @rdname repo_file
#'
#' @export
#'
repo_get_readme = function(repo, branch = NULL, include_details = TRUE) {
  arg_is_chr_scalar(repo)
  arg_is_chr_scalar(branch, allow_null=TRUE)

  file = purrr::possibly(github_api_repo_get_readme, NULL)(repo, branch)
  extract_content(repo, path = "README.md", file = file, include_details = include_details)
}
