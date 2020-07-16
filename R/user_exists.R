github_api_get_user = function(user) {
  arg_is_chr_scalar(user)

  ghclass_api_v3_req(
    endpoint = "/users/:username",
    username = user
  )
}

#' @rdname user
#' @export
#'
user_exists = function(user) {
  arg_is_chr(user)

  res = purrr::map(user, purrr::safely(github_api_get_user))
  purrr::map_lgl(res, succeeded)
}
