mirror_push_repo = function(repo_dir, remote,
                            git = require_git(),
                            options = character(),
                            verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  purrr::walk2(
    repo_dir, remote,
    function(dir, remote) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "push", c("--mirror", get_repo_url(remote), options), verbose = verbose
      )

      cur_dir = fs::path_file(dir)

      status_msg(
        res,
        glue::glue("Pushed (mirror) {usethis::ui_value(cur_dir)} to repo {usethis::ui_value(remote)}."),
        glue::glue("Failed to push (mirror) {usethis::ui_value(cur_dir)} to repo {usethis::ui_value(remote)}.")
      )
    }
  )
}
