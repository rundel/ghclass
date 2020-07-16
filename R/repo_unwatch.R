github_api_repo_unwatch = function(repo){
  ghclass_api_v3_req(
    endpoint = "DELETE /repos/:owner/:repo/subscription",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )

}

#' @rdname repo_notification
#' @export
#'
repo_unwatch = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_unwatch)(repo)

      status_msg(
        res,
        "Unwatched repo {.val {repo}}.",
        "Failed to unwatch repo {.val {repo}}."
      )
    }
  )
}
