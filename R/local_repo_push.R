#' @rdname local_repo
#' @export
local_repo_push = function(repo_dir, remote = "origin", branch = "master",
                           force = FALSE, prompt = TRUE, mirror = FALSE,
                           verbose = FALSE) {
  require_gert()

  arg_is_chr(repo_dir, remote, branch)
  arg_is_lgl_scalar(verbose)

  dir = repo_dir_helper(repo_dir)

  res = purrr::pmap(
    list(dir, remote, branch),
    function(dir, remote, branch) {

      run = TRUE
      if (prompt & force) {
        remotes = gert::git_remote_list(repo_dir)
        repo = remotes$url[remotes$name == remote]

        run = usethis::ui_yeah( paste(
          "This command will overwrite the branch",
          "{usethis::ui_value(branch)} from repo {usethis::ui_value(repo)}."
        ) )
      }

      if (run) {
        res = purrr::safely(gert::git_push)(
          remote = remote,
          refspec = glue::glue("refs/heads/{branch}:refs/heads/{branch}"),
          repo = dir, force = force, mirror = mirror,
          verbose = verbose
        )
      } else {
        res = purrr::safely(usethis::ui_stop)("User canceled (force push) overwrite of branch.")
      }

      repo = fs::path_file(dir)
      ref = paste(remote, branch, sep="/")
      status_msg(
        res,
        glue::glue("Pushed from {usethis::ui_value(fs::path_file(repo))} to {usethis::ui_value(ref)}."),
        glue::glue("Failed to push from {usethis::ui_value(repo)} to {usethis::ui_value(ref)}.")
      )

      res
    }
  )

  invisible(res)
}
