github_api_repo_subscribe = function(repo, subscribed, ignored){
  arg_is_chr_scalar(repo)
  arg_is_lgl_scalar(subscribed, ignored)

  if (subscribed == ignored)
    cli_stop("{.code subscribed != ignored} must be true")

  ghclass_api_v3_req(
    endpoint = "PUT /repos/:owner/:repo/subscription",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    subscribed = subscribed,
    ignored = ignored
  )
}

#' @rdname repo_notification
#' @export
#'
repo_watch = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo, notifications) {
      res = purrr::safely(github_api_repo_subscribe)(
        repo,
        subscribed = TRUE,
        ignored = FALSE
      )

      status_msg(
        res,
        "Watched {.val {repo}}.",
        "Failed to watch {.val {repo}}."
      )
    }
  )
}

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

