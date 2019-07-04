github_api_repo_delete = function(repo) {
  gh::gh("DELETE /repos/:owner/:repo",
         owner = get_repo_owner(repo),
         repo = get_repo_name(repo),
         .token = github_get_token())
}

#' Delete repository
#'
#' `repo_delete` deletes an existing repository from GitHub.
#'
#' @param repo Character. Name of the GitHub repository in `owner/name` format.
#' @param prompt Logical. Should the user be prompted before deleting repositories. Default `true`.
#'

#' @export
#'
repo_delete = function(repo, prompt = TRUE) {

  arg_is_chr(repo, allow_null = TRUE)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = usethis::ui_yeah( paste(
      "This command will delete the following repositories permanently:",
      "{usethis::ui_value(repo)}."
    ) )
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
        glue::glue("Deleted repo {usethis::ui_value(repo)}."),
        glue::glue("Failed to delete repo {usethis::ui_value(repo)}.")
      )
    }
  )
}
