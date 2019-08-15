github_api_user_add = function(repo, username, permission){
  gh::gh("PUT /repos/:owner/:repo/collaborators/:username",
         owner = get_repo_owner(repo),
         repo = get_repo_name(repo),
         username = username,
         permission = permission,
         .token = github_get_token())
}


#' Add a user or team to a repository
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param user Character. One or more GitHub usernames.
#' @param team Character. One or more GitHub team names.
#' @param permission Character. Permission to be granted to team for repo ("push", "pull", or "admin"), defaults to "push".
#'
#' `pull` results in read privileges, `push` in write privileges,
#' and `admin` in Admin privileges for the team in the respective repository.
#' Note that permissions will overwrite existing access privileges.
#'
#' @examples
#' \dontrun{
#' repo_add_user("ghclass-test/hw1-team1", c("user01", "user02"))
#'
#' ## Adding users to their individual repositories
#' user = c("user01", "user02")
#' repo_add_user(paste0("ghclass-test/hw2-", user), user)
#' }
#'
#' @rdname repo_add_member
#'
#' @aliases add_user_to_repo add_team_to_repo
#'
#' @export
repo_add_user = function(repo, user,
                            permission = c("push", "pull", "admin")) {

  permission = match.arg(permission)
  arg_is_chr(repo, user)

  purrr::walk2(
    repo, user,
    function(repo, user) {
      res = purrr::safely(github_api_user_add)(
        repo = repo,
        username = user,
        permission = permission
      )

      status_msg(
        res,
        glue::glue("Added user {usethis::ui_value(user)} to repo {usethis::ui_value(repo)}."),
        glue::glue("Failed to add user {usethis::ui_value(user)} to repo {usethis::ui_value(repo)}.")
      )
    }
  )
}
