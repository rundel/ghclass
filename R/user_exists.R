github_api_get_user = function(user) {
  arg_is_chr_scalar(user)

  gh::gh(
    "/users/:username",
    username = user,
    .token = github_get_token()
  )
}

#' Check if username(s) exists
#'
#' `user_exists` returns TRUE if the supplied username(s) exists on GitHub and FALSE otherwise.
#'
#' @param user Character. Username to be checked. Can be a vector or list of usernames.
#'
#' @return TRUE or FALSE
#'
#' @examples
#' \dontrun{
#' user_exists(c("rundel","hopefullydoesnotexist"))
#' }
#'
#' @aliases check_user_exists
#'
#' @export
#'
user_exists = function(user) {
  arg_is_chr(user)

  res = purrr::map(user, github_api_get_user)
  purrr::map_lgl(res, succeeded)
}
