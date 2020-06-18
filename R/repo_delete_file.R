github_api_repo_delete_file = function(repo, path, message, sha, branch) {
  gh::gh(
    "DELETE /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    path = path,
    message = message,
    sha = sha,
    branch = branch
  )
}


#' Function for deleting files from a Github repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param path Character. File path within the repository.
#' @param message Character. Commit message. If not provided, a custom character string will be created.
#' @param branch Character. Name of branch to use, defaults to "master".
#'
#' @export
#'
repo_delete_file = function(repo, path, message = NULL, branch = "master") {

  arg_is_chr(repo, path, branch)
  arg_is_chr_scalar(message, allow_null = TRUE)


  invisible(purrr::pmap(
    list(repo, path, branch),
    function(repo, path, branch) {
      if (is.null(message))
        message = glue::glue("Deleting file: {path}")

      res = purrr::safely(
        function() {
          sha = withr::with_options(
            list(usethis.quiet = TRUE),
            {
              cur_file = repo_get_file(repo, path, branch)
              repo_txt = format_repo(repo, branch, path)
              if (is.null(cur_file)) {
                cli_stop("Unable to find file {.val {repo_txt}}.")
              }
              attr(cur_file, "sha")
            }
          )

          github_api_repo_delete_file(repo, path, message, sha, branch)
        }
      )()

      status_msg(
        res,
        "Deleted file {.val {path}} from repo {.val {repo}}.",
        "Failed to delete file {.val {path}} from repo {.val {repo}}."
      )

      res
    }
  ))
}
