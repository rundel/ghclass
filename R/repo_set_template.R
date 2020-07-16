github_api_repo_edit = function(repo, ...) {
  arg_is_chr_scalar(repo)

  ghclass_api_v3_req(
    endpoint = "PATCH /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ...,
    # Needed for template repos
    .send_headers = c(Accept = "application/vnd.github.baptiste-preview+json")
  )
}

#' @rdname repo_core
#' @param status Logical. Should the repository be set as a template repository
#' @export
#'
repo_set_template = function(repo, status = TRUE) {

  arg_is_chr(repo)
  arg_is_lgl(status)

  # Checking if repo exists
  purrr::walk2(
    repo, status,
    function(repo, status) {
      res = purrr::safely(github_api_repo_edit)(repo, is_template = status)
      status_msg(
        res,
        "Changed the template status of repo {.val {repo}} to {.val {status}}.",
        "Failed to change template status of repo {.val {repo}}."
      )
    }
  )
}
