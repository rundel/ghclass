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

  d = tibble::tibble(repo, branch)

  res = status_scope(
    "Deleting branches", nrow(d),
    done = "Deleted {n_ok} of {total} branch{?es}",
    purrr::pmap(
      d,
      function(repo, branch) {
        res = purrr::safely(github_api_branch_delete)(repo, branch)

        status_msg(
          res,
          "Deleted branch {.val {format_repo(repo, branch)}}.",
          "Failed to delete branch {.val {format_repo(repo, branch)}}."
        )

        res
      }
    )
  )

  invisible(res)
}

#' @rdname branch
#' @export
#'
branch_remove = function(repo, branch) {
  .Deprecated("branch_delete")
  branch_delete(repo, branch)
}
