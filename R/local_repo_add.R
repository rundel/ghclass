#' @rdname local_repo
#' @export
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

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(files)} to {usethis::ui_value(dir)}."),
        glue::glue("Failed to add {usethis::ui_value(files)} to {usethis::ui_value(dir)}.")
      )

      res
    }
  )

  iinvisible(res)
}
