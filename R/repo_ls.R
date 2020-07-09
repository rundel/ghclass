#' @rdname repo_file
#'
#' @param full_path Logical. Should the function return the full path of the files and directories.
#'
#' @export
#'
repo_ls = function(repo, path = ".", branch = "master", full_path = FALSE) {
  arg_is_chr_scalar(repo, path, branch)
  arg_is_lgl_scalar(full_path)

  res = purrr::safely(github_api_repo_get_file)(repo, path, branch)

  if (failed(res)) {
    status = error(res)[['headers']][['status']]

    cli_stop(
      "Failed to retrieve path {.val {path}} in repo {.val {repo}}.",
      " ({.val status})"
    )
  }
  files = purrr::map_chr(result(res), "path")
  if (!full_path)
    files = fs::path_file(files)

  files
}
