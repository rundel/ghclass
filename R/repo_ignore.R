#' @rdname repo_notification
#' @export
#'
repo_ignore = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo, notifications) {
      res = purrr::safely(github_api_repo_subscribe)(
        repo,
        subscribed = FALSE,
        ignored = TRUE
      )

      status_msg(
        res,
        "Ignored repo {.val {repo}}.",
        "Failed to ignore repo {.val {repo}}."
      )
    }
  )
}
