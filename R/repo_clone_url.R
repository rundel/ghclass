github_api_repo = function(repo) {
  arg_is_chr_scalar(repo)

  gh::gh(
    "GET /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token()
  )
}

#' Get the cloning url for a GitHub repo
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param type Character. Either https or ssh.
#'
#' @return A character vector of urls
#'
#' @export
#'
repo_clone_url = function(repo, type = c("https", "ssh")) {

  arg_is_chr(repo)
  type = match.arg(type)

  details = purrr::map(repo, purrr::safely(github_api_repo))

  if (type == "https")
    purrr::map_chr(details, c("result", "clone_url"), .default = NA)
  else
    purrr::map_chr(details, c("result", "ssh_url"), .default = NA)
}
