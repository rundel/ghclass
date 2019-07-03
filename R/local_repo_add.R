#' @rdname local_repo
#' @export
local_repo_add = function(repo_dir, files = ".",
                    git = require_git(), options = character(),
                    verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk(
    repo_dir,
    function(dir) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "add", c(files, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(files)} to {usethis::ui_value(dir)}."),
        glue::glue("Failed to add {usethis::ui_value(files)} to {usethis::ui_value(dir)}.")
      )
    }
  )
}
