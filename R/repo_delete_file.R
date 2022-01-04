github_api_repo_delete_file = function(repo, path, message, sha = NULL, branch = NULL) {

  if (is.null(sha)) {
    cur_file = repo_get_file(repo, path, branch, quiet = TRUE)
    sha = attr(cur_file, "sha")
  }

  if (is.null(sha)) {
    cli_stop("Unable to locate file using the given path.")
  }

  ghclass_api_v3_req(
    "DELETE /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    path = path,
    message = message,
    sha = sha,
    branch = branch
  )
}


#' @rdname repo_file
#'
#' @export
#'
repo_delete_file = function(repo, path, message = NULL, branch = NULL) {
  arg_is_chr(repo, path)
  arg_is_chr(branch, allow_null = TRUE)
  arg_is_chr_scalar(message, allow_null = TRUE)

  if (is.null(branch))
    branch = list(NULL)

  res = purrr::pmap(
    list(repo, path, branch),
    function(repo, path, branch) {
      if (is.null(message))
        message = cli_glue("Deleting file {path}")

      res = purrr::safely(
        function() {
          github_api_repo_delete_file(repo, path, message, branch = branch)
        }
      )()

      status_msg(
        res,
        "Deleted file {.file {path}} from repo {.val {repo}}.",
        "Failed to delete file {.file {path}} from repo {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}
