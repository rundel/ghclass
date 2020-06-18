#' @rdname local_repo
#' @export
local_repo_pull = function(repo_dir, remote="origin", branch="master", verbose = FALSE) {
  require_gert()

  arg_is_chr(repo_dir, remote, branch)
  arg_is_lgl_scalar(verbose)

  dir = repo_dir_helper(repo_dir)

  res = purrr::pmap(
    list(dir, remote, branch),
    function(dir, remote, branch) {
      withr::local_dir(dir)

      res = purrr::safely(gert::git_pull)(
        remote = remote, repo = dir, verbose = verbose
      )

      repo = fs::path_file(dir)
      status_msg(
        res,
        "Pulled to {.val {repo}}.",
        "Failed to pull to {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}


