github_api_repo_delete = function(repo) {
  ghclass_api_v3_req(
    endpoint = "DELETE /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}

#' @rdname repo_core
#' @param prompt Logical. Should the user be prompted before deleting repositories. Default `true`.
#' @export
#'
repo_delete = function(repo, prompt = TRUE) {

  arg_is_chr(repo, allow_null = TRUE)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = cli_yeah("This command will delete the following repositories permanently: {.val {repo}}.")
    if (!delete) {
      return(invisible())
    }
  }

  purrr::walk(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_delete)(repo)

      status_msg(
        res,
        "Deleted repo {.val {repo}}.",
        "Failed to delete repo {.val {repo}}."
      )
    }
  )
}
