#' @rdname ghclass-internal
#' @export
read_bin_file = function(x) {
  f = file(x, "rb")
  size = file.size(x)
  raw = readBin(f, "raw", size)
  close(f)

  raw
}


#' @rdname repo_file
#'
#' @param file Character. Local file path(s) of file or files to be added.
#' @param repo_folder Character. Name of folder on repository to save the file(s) to. If the folder does not exist on the repository, it will be created.
#' @param preserve_path Logical. Should the local relative path be preserved. Default `FALSE`.
#' @param overwrite Logical. Should existing file or files with same name be overwritten. Default `FALSE`.
#'
#' @export
#'
repo_add_file = function(repo, file, message = NULL, repo_folder = NULL, branch = NULL,
                         preserve_path = FALSE, overwrite = FALSE) {

  arg_is_chr(repo, file)
  arg_is_chr(branch, allow_null=TRUE)
  arg_is_chr_scalar(repo_folder, message, allow_null = TRUE)
  arg_is_lgl_scalar(preserve_path, overwrite)

  missing_files = file[!fs::file_exists(file)]
  if (length(missing_files) != 0)
    cli_stop("Unable to locate the following file{?s}: {.val {missing_files}}")

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  if (is.null(branch))
    branch = list(NULL)

  res = purrr::pmap(
    list(repo, file, branch),
    function(repo, file, branch) {
      purrr::map(
        file,
        function(file){
          gh_path = file

          if (!preserve_path)
            gh_path = fs::path_file(file)

          if(!is.null(repo_folder))
            gh_path = fs::path(repo_folder, gh_path)

          if (!file_exists(repo, gh_path, branch) | overwrite) {
            res = repo_put_file(
              repo = repo,
              path = gh_path,
              content = read_bin_file(file),
              message = message,
              branch = branch,
              verbose = FALSE
            )
          } else {
            res = failed_result(
              "File already exists, re-run with overwrite = TRUE to replace it."
            )
          }

          status_msg(
            res,
            "Added file {.val {gh_path}} to repo {.val {repo}}.",
            "Failed to add file {.val {gh_path}} to repo {.val {repo}}."
          )
        }
      )
    }
  )

  invisible(res)
}
