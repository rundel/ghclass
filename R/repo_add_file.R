read_bin_file = function(x) {
  f = file(x, "rb")
  size = file.size(x)
  raw = readBin(f, "raw", size)
  close(f)

  raw
}


#' Add files to a repo
#'
#' `repo_add_file` uses the GitHub API to add/update files in an existing repo on GitHub. Note that due to time delays in caching, files that have been added very recently might not yet be displayed as existing and might accidentally be overwritten.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param file Character. Local file path(s) of file or files to be added.
#' @param message Character. Commit message. If not provided, a custom character string will be created, in the form of "Added file(s): filename(s)". If this custom message character length exceeds 50, it will be shortened to "Added file(s)".
#' @param repo_folder Character. Name of folder on repository to save the file(s) to. If the folder does not exist on the repository, it will be created.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param preserve_path Logical. Should the local relative path be preserved.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' repo_add_file("rundel/ghclass", "./DESCRIPTION", "Update DESCRIPTION")
#' }
#'
#' @export
#'
repo_add_file = function(repo, file, message = NULL, repo_folder = NULL, branch = "master",
                         preserve_path = FALSE, overwrite = FALSE) {

  arg_is_chr(repo, file, branch)
  arg_is_chr_scalar(repo_folder, message, allow_null = TRUE)
  arg_is_lgl_scalar(preserve_path, overwrite)

  missing_files = file[!fs::file_exists(file)]
  if (length(missing_files) != 0)
    cli_stop("Unable to locate the following file{?s}: {.val {missing_files}}")

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  purrr::pwalk(
    list(repo, file, branch),
    function(repo, file, branch) {
      purrr::walk(
        file,
        function(file){
          gh_path = file

          if (!preserve_path)
            gh_path = fs::path_file(file)

          if(!is.null(repo_folder))
            gh_path = fs::path(repo_folder, gh_path)

          if (!file_exists(repo, gh_path, branch) | overwrite) {
            repo_put_file(
              repo = repo,
              path = gh_path,
              content = read_bin_file(file),
              message = message,
              branch = branch,
              verbose = TRUE
            )
          } else {

            cli::cli_alert_danger( c(
              "Failed to add file {.val {gh_path}} to repo {.val {repo}}, this file already exists. ",
              "If you want to force add this file, re-run the command with {.code overwrite = TRUE}."
            ) )
          }
        }
      )
    }
  )
}
