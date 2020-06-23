#' @rdname local_repo
#' @export
#'
local_repo_pull = function(repo_dir, branch="master", verbose = FALSE) {
  # TODO - add support for remotes when added to gert
  require_gert()

  arg_is_chr(repo_dir, branch)
  arg_is_lgl_scalar(verbose)

  dir = repo_dir_helper(repo_dir)

  res = purrr::pmap(
    list(dir, branch),
    function(dir, branch) {
      withr::local_dir(dir)

      res = purrr::safely(gert::git_pull)(
        repo = dir, verbose = verbose
      )

      repo = fs::path_file(dir)
      status_msg(
        res,
        "Pulled to local repo {.val {repo}}.",
        "Failed to pull to local repo {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}


