#' @rdname local_repo
#' @export
local_repo_commit = function(repo_dir, message,
                       git = require_git(), options = character(),
                       verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))
  stopifnot(!missing(message))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk2(
    repo_dir, message,
    function(dir, message) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "commit", c("-m", message, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Committed {usethis::ui_value(dir)}."),
        glue::glue("Failed to commit {usethis::ui_value(dir)}.")
      )
    }
  )
}
