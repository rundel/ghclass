#' @rdname repo_core
#' @export
#'
repo_allows_forking = function(repo) {
  arg_is_chr(repo)

  status_scope(
    "Retrieving forking status", length(repo),
    done = "Retrieved forking status for {n_ok} of {total} repo{?s}",
    purrr::map_lgl(
      repo,
      function(repo) {
        res = purrr::safely(github_api_repo)(repo)

        status_msg(
          res,
          fail = "Failed to retrieve forking status for repo {.val {repo}}."
        )

        if (succeeded(res)) {
          result(res)[["allow_forking"]]
        } else {
          NA
        }
      }
    )
  )
}
