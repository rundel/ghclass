github_api_repo_remove_user = function(repo, username){
  ghclass_api_v3_req(
    endpoint = "DELETE /repos/:owner/:repo/collaborators/:username",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    username = username
  )
}


#' @rdname repo_user
#' @export
#'
repo_remove_user = function(repo, user) {
  arg_is_chr(repo, user)

  d = tibble::tibble(repo, user)

  res = status_scope(
    "Removing users from repos", nrow(d),
    done = "Removed {n_ok} of {total} user{?s} from repos",
    purrr::pmap(
      d,
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
  )

  invisible(res)
}
