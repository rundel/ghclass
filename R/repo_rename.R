github_api_repo_rename = function(repo, new_name){
  ghclass_api_v3_req(
    endpoint = "PATCH /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    name = new_name
  )
}

#' @rdname repo_core
#' @param new_repo Character. New name of repository without the owner.
#' @export
#'
repo_rename = function(repo, new_repo) {
  arg_is_chr(repo, new_repo)

  purrr::walk2(
    repo, new_repo,
    function(repo, new_repo) {
      res = purrr::safely(github_api_repo_rename)(repo, new_repo)
      new_full = paste0(get_repo_owner(repo), "/", new_repo)

      status_msg(
        res,
        "Renamed repo {.val {repo}} to {.val {new_full}}.",
        "Failed to rename repo {.val {repo}} to {.val {new_full}}."
      )
    }
  )
}
