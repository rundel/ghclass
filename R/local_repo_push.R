#' @rdname local_repo
#' @export
local_repo_push = function(repo_dir, remote = "origin", branch = "master", verbose = FALSE) {
  require_gert()

  arg_is_chr(repo_dir, remote, branch)
  arg_is_lgl_scalar(verbose)

  dir = repo_dir_helper(repo_dir)

  res = purrr::pmap(
    list(dir, remote, branch),
    function(dir, remote, branch) {
      res = purrr::safely(gert::git_push)(
        remote = remote,
        refspec = glue::glue("refs/heads/{branch}:refs/heads/{branch}"),
        verbose = verbose, repo = dir
      )

      status_msg(
        res,
        glue::glue("Pushed {usethis::ui_value(dir)}."),
        glue::glue("Failed to push {usethis::ui_value(dir)}.")
      )

      res
    }
  )

  invisible(res)
}
