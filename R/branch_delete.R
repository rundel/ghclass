github_api_branch_delete = function(repo, branch) {
  ghclass_api_v3_req(
    endpoint = "DELETE /repos/:owner/:repo/git/refs/:ref",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ref = paste0("heads/", branch)
  )
}


#' @rdname branch
#' @export
#'
branch_delete = function(repo, branch) {
  arg_is_chr(repo, branch)

  purrr::pwalk(
    list(repo, branch),
    function(repo, branch) {
      res = purrr::safely(github_api_branch_delete)(repo, branch)

      status_msg(
        res,
        "Removed branch {.val {format_repo(repo, branch)}}.",
        "Failed to remove branch {.val {format_repo(repo, branch)}}."
      )
    }
  )
}

#' @rdname branch
#' @export
#'
branch_remove = function(repo, branch) {
  .Deprecated("branch_delete")
  branch_delete(repo, branch)
}
