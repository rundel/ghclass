github_api_user_repos = function(owner, type) {
  arg_is_chr_scalar(owner)

  gh::gh(
    "GET /users/:owner/repos",
    owner = owner,
    type = type,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

github_api_your_repos = function(type) {
  gh::gh(
    "GET /user/repos",
    type = type,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' Get user's repository
#'
#' `user_repos` returns a (filtered) vector of repositories belonging to a GitHub user.
#'
#' @param user Character. Username of the GitHub user.
#' @param type Characer. Can be one of "all", "owner", "public", "private", "member".
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param full_repo Logical. Should the full repository address be returned (e.g. `owner/name` instead of just `repo`)?
#'
#'
#' @examples
#' \dontrun{
#' user_repos("rundel")
#' user_repos("rundel", "ghclass")
#' }
#'
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

  status_msg(res, fail = "Failed to retrieve repos for user {usethis::ui_value(user)}.")

  if (failed(res))
    return(invisible(NULL))

  if (full_repo) {
    res = purrr::map_chr(result(res), "full_name")
  } else {
    res = purrr::map_chr(result(res), "name")
  }

  filter_results(res, filter, exclude)
}
