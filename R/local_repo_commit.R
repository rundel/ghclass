#' @rdname local_repo
#' @export
local_repo_commit = function(repo_dir, message) {
  require_gert()
  arg_is_chr(repo_dir, message)

  repo_dir = repo_dir_helper(repo_dir)

  d = tibble::tibble(dir = repo_dir, message)

  res = status_scope(
    "Committing changes", nrow(d),
    done = "Committed changes to {n_ok} of {total} repo{?s}",
    purrr::pmap(
      d,
      function(dir, message) {
        res = purrr::safely(gert::git_commit)(
          message = message, repo = dir
        )

        repo = fs::path_file(dir)
        status_msg(
          res,
          "Committed changes to {.val {repo}}.",
          "Failed to commit changes to {.val {repo}}."
        )

        res
      }
    )
  )

  invisible(res)
}
