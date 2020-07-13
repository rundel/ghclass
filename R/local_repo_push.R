#' @rdname local_repo
#' @export
#'
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
      repo = fs::path_file(dir)
      ref = paste(remote, branch, sep="/")

      run = TRUE
      if (prompt & force) {
        remotes = gert::git_remote_list(repo_dir)
        repo = remotes$url[remotes$name == remote]

        run = cli_yeah("This command will overwrite the branch {.val {branch}} from repo {.val {repo}}.")
      }

      if (run) {
        res = purrr::safely(gert::git_push)(
          remote = remote,
          refspec = glue::glue("refs/heads/{branch}:refs/heads/{branch}"),
          repo = dir, force = force, mirror = mirror,
          verbose = verbose
        )
      } else {
        cli::cli_alert_danger("User canceled force push (overwrite) of {.val {ref}}")
      }

      status_msg(
        res,
        "Pushed from local repo {.val {fs::path_file(repo)}} to {.val {ref}}.",
        "Failed to push from local repo {.val {repo}} to {.val {ref}}."
      )

      res
    }
  )

  invisible(res)
}
