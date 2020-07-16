#' @rdname local_repo
#' @export
#'
local_repo_add = function(repo_dir, files = ".") {
  require_gert()
  arg_is_chr(repo_dir, files)
  repo_dir = repo_dir_helper(repo_dir)

  res = purrr::map(
    repo_dir,
    function(dir) {
      if (files == ".") {
        files = gert::git_status(repo = dir)[["file"]]
      }

      res = purrr::safely(gert::git_add)(
        files = files, repo = dir
      )

      repo = fs::path_file(dir)
      status_msg(
        res,
        "Added files {.val {files}} to {.val {repo}}.",
        "Failed to add files {.val {files}} to {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}
