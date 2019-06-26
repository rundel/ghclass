# GitHub API call to get a list of repos user is watching
github_api_get_watching = function(){
  gh(
    "GET /user/subscriptions",
    .token = get_github_token(),
    .limit = get_github_api_limit()
  )
}

#' Get repos user is watching
#'
#' Returns all of the authenticated user's watched repositories. This should
#' match the list at [github.com/watching](https://github.com/watching).
#' The function can also filter the results for matching (or excluding)
#' repositories.
#'
#' @param filter character, regex pattern for matching (or excluding) repositories.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_watching()
#' get_watching("Sta523-Fa15")
#' }
#'
#' @family notification functions
#'
#' @export
#'
get_watching = function(filter = NULL, exclude = FALSE){
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::map_chr(github_api_get_watching(), "full_name")
  filter_results(res, filter, exclude)
}


github_api_unwatch_repo = function(repo){
  gh::gh(
    "DELETE /repos/:owner/:repo/subscription",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = get_github_token()
  )

}

#' Unwatch repository
#'
#' Unwatches / unsubscribes from the provided GitHub repositories.
#'
#' @param repo github repository address in `owner/repo` format
#'
#' @examples
#' \dontrun{
#' unwatch_repo()
#' unwatch_repo("Sta523-Fa15/hw1-Tim")
#' }
#'
#' @family notification functions
#'
#' @export
#'
unwatch_repo = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo) {
      res = purrr::safely(github_api_unwatch_repo)(repo)

      status_msg(
        res,
        glue::glue("Unwatched {usethis::ui_value(repo)}."),
        glue::glue("Failed to unwatch {usethis::ui_value(repo)}.")
      )
    }
  )
}




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
    .token = get_github_token()
  )
}

#' Watch repository
#'
#' Watches / subscribes to the provided GitHub repositories.
#'
#' @param repo github repository address in `owner/repo` format
#'
#' @family notification functions
#'
#' @export
#'
watch_repo = function(repo) {
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


#' Ignore repository
#'
#' Ignores the provided GitHub repositories.
#'
#' @param repo github repository address in `owner/repo` format
#'
#' @family notification functions
#'
#' @export
#'
ignore_repo = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo, notifications) {
      res = purrr::safely(github_api_set_subscription)(
        repo,
        subscribed = FALSE,
        ignored = TRUE
      )

      status_msg(
        res,
        glue::glue("Ignored {usethis::ui_value(repo)}."),
        glue::glue("Failed to ignore {usethis::ui_value(repo)}.")
      )
    }
  )
}
