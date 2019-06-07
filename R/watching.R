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
#' @family github repo related functions
#'
#' @export
get_watching = function(filter = NULL, exclude = FALSE){

  stopifnot(length(filter) <= 1)

  res = purrr::map_chr(github_api_get_watching(), "full_name")
  filter_results(res, filter, exclude)

}

# GitHub API call to unwatch a repo
github_api_unwatch_repo = function(repo){

  require_valid_repo(repo)

  owner = get_repo_owner(repo)
  name = get_repo_name(repo)

  gh(
    "DELETE /repos/:owner/:repo/subscription",
    owner = owner, repo = name,
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
#' @family github repo related functions
#'
#' @export
unwatch_repo = function(repo) {

  purrr::walk(
    repo,
    function(repo) {
      res = purrr::safely(github_api_unwatch_repo)(repo)

      repo_fmt = usethis::ui_value(repo)

      status_msg(
        res,
        glue::glue("Unwatched {repo_fmt}."),
        glue::glue("Failed to unwatch {repo_fmt}.")
      )
    }
  )
}



# GitHub API change a repo subscription
github_api_set_subscription = function(repo, subscribed, ignored){

  if (subscribed ==ignored)
    usethis::ui_stop("{usethis::ui_code('subscribed != ignored')} must be true")

  require_valid_repo(repo)

  owner = get_repo_owner(repo)
  name = get_repo_name(repo)

  gh(
    "PUT /repos/:owner/:repo/subscription",
    owner = owner, repo = name,
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
#' @family github repo related functions
#'
#' @export
watch_repo = function(repo) {

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
#' @family github repo related functions
#'
#' @export
ignore_repo = function(repo) {

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
