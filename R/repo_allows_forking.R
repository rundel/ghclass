#' @rdname repo_core
#' @export
#'
repo_allows_forking = function(repo) {

  arg_is_chr(repo)

  purrr::map_lgl(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo)(repo)
      status_msg(
        res,
        fail = "Failed to retrieve repo {.val {repo}}."
      )

      if (succeeded(res)) {
        result(res)[["allow_forking"]]
      } else {
        NA
      }
    }
  )
}
