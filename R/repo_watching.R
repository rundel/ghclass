github_api_repo_watching = function() {
  gh::gh(
    "GET /user/subscriptions",
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' @rdname repo_notification
#'
#' @param filter character, regex pattern for matching (or excluding) repositories.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @export
#'
repo_watching = function(filter = NULL, exclude = FALSE){
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude)

  res = purrr::map_chr(github_api_repo_watching(), "full_name")
  filter_results(res, filter, exclude)
}
