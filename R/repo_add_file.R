#' Add files to a repo
#'
#' `repo_add_file` uses the GitHub API to add/update files in an existing repo on GitHub. Note that due to time delays in caching, files that have been added very recently might not yet be displayed as existing and might accidentally be overwritten.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param message Character. Commit message. If not provided, a custom character string will be created, in the form of "Added file(s): filename(s)". If this custom message character length exceeds 50, it will be shortened to "Added file(s)".
#' @param file Character. Local file path(s) of file or files to be added.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param preserve_path Logical. Should the local relative path be preserved.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' repo_add_file("rundel/ghclass", "Update DESCRIPTION", "./DESCRIPTION")
#' }
#'
#' @family file functions
#'
#' @export
#'
repo_add_file = function(repo, file, message = NULL, branch = "master",
                    preserve_path = FALSE, overwrite = FALSE) {

  arg_is_chr(repo, file, branch)
  arg_is_chr(message, allow_null = TRUE)
  arg_is_lgl_scalar(preserve_path, overwrite)

  file_status = fs::file_exists(file)
  if (any(!file_status))
    usethis::ui_stop("Unable to locate the following file(s): {usethis::ui_value(file)}")

  if (is.null(message))
    message = list(NULL)

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  purrr::pwalk(
    list(repo, file, message, branch),
    function(repo, file, message, branch){
      purrr::walk(
        file,
        function(file){
          gh_path = file
          if(!preserve_path)
            gh_path = fs::path_file(file)

          if(!check_file_modification(repo, gh_path, branch) | overwrite){
            repo_put_file(
              repo = repo,
              path = gh_path,
              content = paste(readLines(file), collapse = "\n"),
              message = message,
              branch = branch,
              verbose = TRUE
            )
          } else {
            usethis::ui_oops( paste(
              'Failed to add file {usethis::ui_value(gh_path)} to repo {usethis::ui_value(repo)}, file already exists.',
              'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
            ) )
          }
        })
    })
}
