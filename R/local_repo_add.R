#' @rdname local_repo
#' @export
local_repo_add = function(repo_dir, files = ".", verbose = FALSE) {
  require_gert()

  arg_is_chr(repo_dir, files)
  arg_is_lgl_scalar(verbose)

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk(
    repo_dir,
    function(dir) {
      res = purrr::safely(gert::git_add)(
        files = files, repo = dir, verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(files)} to {usethis::ui_value(dir)}."),
        glue::glue("Failed to add {usethis::ui_value(files)} to {usethis::ui_value(dir)}.")
      )
    }
  )
}
