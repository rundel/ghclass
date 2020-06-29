github_api_get_user = function(user) {
  arg_is_chr_scalar(user)

  gh::gh(
    "/users/:username",
    username = user,
    .token = github_get_token()
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
