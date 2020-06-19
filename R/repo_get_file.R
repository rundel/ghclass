github_api_repo_get_file = function(repo, path, branch) {
  arg_is_chr_scalar(repo, path, branch)

  gh::gh(
    "GET /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    path = path,
    ref = branch,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )

}


#' Low level function for retrieving a file from a GitHub Repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param path Characer. Path to the file within the repository.
#' @param branch Character. Name of branch to use, defaults to "master".
#'
#' @export
#'
repo_get_file = function(repo, path, branch = "master", quiet = FALSE) {
  arg_is_chr_scalar(repo, path, branch)

  file = purrr::possibly(github_api_repo_get_file, NULL)(repo, path, branch)
  extract_content(repo = repo, path = path, file = file, quiet = quiet)
}
