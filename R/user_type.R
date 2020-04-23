#' Check type of a username
#'
#' `user_type` returns a Character vector of the accounts' types.
#'
#' @param user Character. Username(s) to be checked.
#'
#' @examples
#' \dontrun{
#' user_type(c("rundel","ghclass-demo"))
#' }
#'
#' @export
#'
user_type = function(user) {
  arg_is_chr(user)

  res = purrr::map(user, purrr::safely(github_api_get_user))
  purrr::map_chr(res, c("result","type"), .default=NA)
}
