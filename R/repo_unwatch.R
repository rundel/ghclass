github_api_unwatch_repo = function(repo){
  gh::gh(
    "DELETE /repos/:owner/:repo/subscription",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token()
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
#' @family notification
#'
#' @export
#'
unwatch_repo = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo) {
      res = purrr::safely(github_api_unwatch_repo)(repo)

      status_msg(
        res,
        glue::glue("Unwatched {usethis::ui_value(repo)}."),
        glue::glue("Failed to unwatch {usethis::ui_value(repo)}.")
      )
    }
  )
}
