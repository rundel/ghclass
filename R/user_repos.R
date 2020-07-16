github_api_user_repos = function(owner, type) {
  arg_is_chr_scalar(owner)

  ghclass_api_v3_req(
    endpoint = "GET /users/:owner/repos",
    owner = owner,
    type = type
  )
}

github_api_your_repos = function(type) {
  ghclass_api_v3_req(
    endpoint = "GET /user/repos",
    type = type
  )
}

#' @rdname user
#' @export
#'
user_repos = function(user, type = c("owner", "all", "public", "private", "member"),
                      filter = NULL, exclude = FALSE, full_repo = TRUE) {

  type = match.arg(type)

  arg_is_chr_scalar(user, type)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(
    function() {
      if (user == github_whoami()) {
        github_api_your_repos(type)
      } else {
        github_api_user_repos(user, type)
      }
    }
  )()

  status_msg(res, fail = "Failed to retrieve repos for user {.val {user}}.")

  if (failed(res))
    return(invisible(NULL))

  if (full_repo) {
    res = purrr::map_chr(result(res), "full_name")
  } else {
    res = purrr::map_chr(result(res), "name")
  }

  filter_results(res, filter, exclude)
}
