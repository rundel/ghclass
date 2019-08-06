github_api_repo_get_file = function(repo, file, branch) {
  arg_is_chr_scalar(repo, file, branch)

  gh::gh(
    "GET /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    path = file,
    ref = branch,
    .token = github_get_token(),
    .limit = get_github_api_limit()
  )

}


#' Low level function for retrieving a file from a GitHub Repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param file Characer. Path to the file within the repository.
#' @param branch Character. Name of branch to use, defaults to "master".
#'

#' @aliases get_file
#'
#' @export
#'
repo_get_file = function(repo, file, branch = "master") {
  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)
  stopifnot(length(branch) == 1)

  file = purrr::possibly(github_api_repo_get_file, NULL)(repo, file, branch)

  extract_content(file)
}
