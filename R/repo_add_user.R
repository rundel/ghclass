github_api_repo_add_user = function(repo, username, permission){
  ghclass_api_v3_req(
    endpoint = "PUT /repos/:owner/:repo/collaborators/:username",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    username = username,
    permission = permission
  )
}



#' @rdname repo_user
#' @export
#'
repo_add_user = function(repo, user, permission = c("push", "pull", "admin", "maintain", "triage")) {
  permission = match.arg(permission)
  arg_is_chr(repo, user)

  purrr::walk2(
    repo, user,
    function(repo, user) {
      res = purrr::safely(github_api_repo_add_user)(
        repo = repo,
        username = user,
        permission = permission
      )

      status_msg(
        res,
        "Added user {.val {user}} to repo {.val {repo}}.",
        "Failed to add user {.val {user}} to repo {.val {repo}}."
      )
    }
  )
}
