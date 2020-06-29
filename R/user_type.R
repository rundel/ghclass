#' @rdname user
#' @export
#'
user_type = function(user) {
  arg_is_chr(user)

  res = purrr::map(user, purrr::safely(github_api_get_user))
  purrr::map_chr(res, c("result","type"), .default=NA)
}
