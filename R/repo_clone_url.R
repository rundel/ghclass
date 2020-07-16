github_api_repo = function(repo) {
  arg_is_chr_scalar(repo)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}

#' @rdname repo_details
#'
#' @param type Character. Clone url type, either "https" or "ssh".
#'
#' @export
#'
repo_clone_url = function(repo, type = c("https", "ssh")) {
  arg_is_chr(repo)
  type = match.arg(type)

  details = purrr::map(repo, purrr::safely(github_api_repo))

  if (type == "https") {
    lookup = c("result", "clone_url")
  } else {
    lookup = c("result", "ssh_url")
  }

  purrr::map_chr(details, lookup, .default = NA)
}
