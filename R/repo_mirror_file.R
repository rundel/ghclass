format_folder = function(folder, path) {
  if (!is.null(folder)) {
    glue::glue("{folder}/{path}")
  } else {
    path
  }
}


#' Mirror file(s) between repos
#'
#' `repo_mirror_file` mirrors select file(s) between repositories.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. Address of repository in "owner/name" format.
#' @param path Character or character vector. Name(s) of file(s) to be moved.
#' @param source_folder Character. Name of folder containing file on `source_repo`.
#' @param target_folder Character. Name of folder containing file on `target_repo`.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#'
repo_mirror_file = function(source_repo,
                            target_repo,
                            path = NULL,
                            source_folder = NULL,
                            target_folder = NULL,
                            message = NULL,
                            branch = "master",
                            overwrite = FALSE,
                            verbose = TRUE) {
  arg_is_chr(path)
  arg_is_chr_scalar(source_repo, target_repo)
  arg_is_chr_scalar(source_folder, target_folder, message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  source_files = repo_files(source_repo, branch)
  target_files = repo_files(target_repo, branch)

  purrr::walk(path,
              function(p) {

                source_path = format_folder(source_folder, p)
                target_path = format_folder(target_folder, p)

                if (source_path %in% source_files[['path']]) {
                  if (!(target_path %in% target_files[['path']]) | overwrite) {
                    res = purrr::safely(repo_get_file)(source_repo, source_path)

                    if (succeeded(res)) {
                      repo_put_file(
                        repo = target_repo,
                        path = target_path,
                        content = res[['result']],
                        message = message,
                        branch = branch,
                        verbose = verbose
                      )
                    }
                  } else {
                    usethis::ui_oops(
                      paste(
                        'Failed to add {usethis::ui_value(target_path)} to {usethis::ui_value(target_repo)}: already exists.',
                        'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
                      )
                    )
                  }
                } else {
                  usethis::ui_oops(
                    "Failed to locate {usethis::ui_value(source_path)} on {usethis::ui_value(source_repo)}."
                  )
                }
              })
}
