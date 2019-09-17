#' @rdname local_repo
#' @export
local_repo_commit = function(repo_dir, message, verbose = FALSE) {
  require_gert()

  arg_is_chr(repo_dir, message)
  arg_is_lgl_scalar(verbose)

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk2(
    repo_dir, message,
    function(dir, message) {
      res = purrr::safely(gert::git_commit)(
        message = message, repo = dir, verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Committed {usethis::ui_value(dir)}."),
        glue::glue("Failed to commit {usethis::ui_value(dir)}.")
      )
    }
  )
}
