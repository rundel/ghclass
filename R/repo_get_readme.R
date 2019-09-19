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

#' Low level function for retrieving the README of a GitHub Repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param branch Character. Name of branch to use, defaults to "master".
#'

#' @aliases get_readme
#'
#' @export
#'
repo_get_readme = function(repo, branch = "master") {
  arg_is_chr_scalar(repo, branch)

  file = purrr::possibly(github_api_repo_get_readme, NULL)(repo, branch)
  extract_content(repo, "README.md", file)
}
