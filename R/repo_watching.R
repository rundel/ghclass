github_api_repo_watching = function() {
  gh::gh(
    "GET /user/subscriptions",
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' Get watched repos
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
#' repo_watching()
#' repo_watching("hw1")
#' }
#'

#' @aliases get_watching
#'
#' @export
#'
repo_watching = function(filter = NULL, exclude = FALSE){
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::map_chr(github_api_repo_watching(), "full_name")
  filter_results(res, filter, exclude)
}
