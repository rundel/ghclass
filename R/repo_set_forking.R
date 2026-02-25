#' @rdname repo_core
#' @param status Logical. Should forking be allowed for the repository.
#' @export
#'
repo_set_forking = function(repo, status = TRUE) {

  arg_is_chr(repo)
  arg_is_lgl(status)

  res = purrr::map2(
    repo, status,
    function(repo, status) {
      res = purrr::safely(github_api_repo_edit)(repo, allow_forking = status)
      status_msg(
        res,
        "Changed the forking status of repo {.val {repo}} to {.val {status}}.",
        "Failed to change forking status of repo {.val {repo}}."
      )
    }
  )

  invisible(res)
}
