github_api_repo_remove_user = function(repo, username, permission){
  gh::gh("DELETE /repos/:owner/:repo/collaborators/:username",
         owner = get_repo_owner(repo),
         repo = get_repo_name(repo),
         username = username,
         permission = permission,
         .token = github_get_token())
}


#' Remove a user from a repository
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param user Character. One or more GitHub usernames.
#'
#' @export
repo_remove_user = function(repo, user) {
  arg_is_chr(repo, user)

  purrr::walk2(
    repo, user,
    function(repo, user) {
      res = purrr::safely(github_api_repo_remove_user)(
        repo = repo,
        username = user
      )

      status_msg(
        res,
        "Removed user {.val {user}} from repo {.val {repo}}.",
        "Failed to remove user {.val {user}} from repo {.val {repo}}."
      )
    }
  )
}
