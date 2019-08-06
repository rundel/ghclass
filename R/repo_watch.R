github_api_set_subscription = function(repo, subscribed, ignored){
  arg_is_chr_scalar(repo)
  arg_is_lgl_scalar(subscribed, ignored)

  if (subscribed == ignored)
    usethis::ui_stop("{usethis::ui_code('subscribed != ignored')} must be true")

  gh::gh(
    "PUT /repos/:owner/:repo/subscription",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    subscribed = subscribed,
    ignored = ignored,
    .token = github_get_token()
  )
}

#' Watch repository
#'
#' Watches / subscribes to the provided GitHub repositories.
#'
#' @param repo github repository address in `owner/repo` format
#'
#' @aliases watch_repo
#'
#' @export
#'
repo_watch = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo, notifications) {
      res = purrr::safely(github_api_set_subscription)(
        repo,
        subscribed = TRUE,
        ignored = FALSE
      )

      status_msg(
        res,
        glue::glue("Watched {usethis::ui_value(repo)}."),
        glue::glue("Failed to watch {usethis::ui_value(repo)}.")
      )
    }
  )
}
