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

  res = purrr::map2(
    repo, user,
    function(repo, user) {
      res = purrr::safely(github_api_repo_add_user)(
        repo = repo,
        username = user,
        permission = permission
      )

      status_msg(
        res,
        "User {.val {user}} given {.val {permission}} access to repo {.val {repo}}",
        "Failed to give user {.val {user}} {.val {permission}} access to repo {.val {repo}}."
      )
    }
  )

  invisible(res)
}

#' @rdname repo_user
#' @export
#'
repo_user_permission = function(
  repo, user,
  permission = c("push", "pull", "admin", "maintain", "triage")
) {
  permission = match.arg(permission)
  arg_is_chr(repo, user)

  repo_add_user(repo = repo, user = user, permission = permission)
}
