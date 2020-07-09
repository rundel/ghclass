#' @rdname repo_core
#' @export
#'
repo_is_template = function(repo) {

  arg_is_chr(repo)

  # Checking if repo exists
  purrr::map_lgl(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo)(repo)
      status_msg(
        res,
        fail = "Failed to retrieve repo {.val {repo}}."
      )

      if (succeeded(res)) {
        result(res)[["is_template"]]
      } else {
        NA
      }
    }
  )
}
