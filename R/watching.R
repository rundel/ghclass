# GitHub API call to get a list of repos user is watching
github_api_get_watching = function(){
  gh(
    "GET /user/subscriptions",
    .token = ghclass::get_github_token(),
    .limit = ghclass::get_github_api_limit()
  )
}

#' Get repos user is watching
#'
#' Returns all of the authenticated user's watched repositories. This should
#' match the list at https://github.com/watching. The function can also
#' filter the results for matching (or excluding) repositories.
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

# GitHub API call to unwatch a repos
github_api_unwatch_repo = function(repo){

  require_valid_repo(repo)

  owner = ghclass::get_repo_owner(repo)
  name = ghclass::get_repo_name(repo)

  gh(
    "DELETE /repos/:owner/:repo/subscription",
    owner = owner, repo = name,
    .token = ghclass::get_github_token()
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
unwatch_repo <- function(repo) {

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
