github_api_repo_put_file = function(repo, path, content, message, branch) {
  args = list(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = path,
    content = base64enc::base64encode(content),
    message = message, branch = branch,
    .token = github_get_token()
  )

  # To update an existing file we need its current SHA,
  # if the file does not exist this will be NULL.
  cur_file = repo_get_file(repo, path, branch)
  args[["sha"]] = attr(cur_file, "sha")

  do.call(gh::gh, args)
}


#' Low level function for adding a file to a Github repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param path Character. File path within the repository.
#' @param content Character or raw. Content of the file.
#' @param message Character. Commit message. If not provided, a custom character string will be created.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param verbose Logical. Should success / failure messages be printed
#'

#' @aliases put_file
#'
#' @export
#'
repo_put_file = function(repo, path, content, message = NULL, branch = "master", verbose = TRUE) {

  arg_is_chr_scalar(repo, path, branch)
  arg_is_chr_scalar(message, allow_null = TRUE)

  if (is.null(message))
    message = glue::glue("Adding file: {path}")

  if (is.character(content))
    content = charToRaw(content)

  res = purrr::safely(github_api_repo_put_file)(repo, path, content, message, branch)

  if(verbose){
    status_msg(
      res,
      glue::glue("Added file {usethis::ui_value(path)} to repo {usethis::ui_value(repo)}."),
      glue::glue("Failed to add file {usethis::ui_value(path)} to repo {usethis::ui_value(repo)}.")
    )
  }

  res
}
