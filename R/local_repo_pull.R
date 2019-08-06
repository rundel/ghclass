#' @rdname local_repo
#' @export
local_repo_pull = function(repo_dir, remote="origin", branch="master",
                     git = require_git(), options = character(),
                     verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  dir = repo_dir_helper(repo_dir)

  purrr::pwalk(
    list(dir, remote, branch),
    function(dir, remote, branch) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "pull", c(remote, branch, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Pulled {usethis::ui_value(dir)}."),
        glue::glue("Failed to pull {usethis::ui_value(dir)}.")
      )
    }
  )
}


