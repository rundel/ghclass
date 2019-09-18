#' @rdname local_repo
#' @export
local_repo_branch = function(repo_dir, branch) {
  require_gert()
  arg_is_chr(repo_dir)
  arg_is_chr_scalar(branch)

  print(repo_dir)

  repo_dir = repo_dir_helper(repo_dir)

  res = purrr::map(
    repo_dir,
    function(dir) {
      res = purrr::safely(gert::git_branch_create)(
        name = branch, repo = dir
      )

      repo = fs::path_file(dir)
      status_msg(
        res,
        glue::glue("Added branch {usethis::ui_value(branch)} to {usethis::ui_value(repo)}."),
        glue::glue("Failed to add branch {usethis::ui_value(branch)} to {usethis::ui_value(repo)}.")
      )

      res
    }
  )

  invisible(res)
}
