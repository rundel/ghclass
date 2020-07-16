github_api_repo_put_file = function(repo, path, content, message, branch, sha = NULL) {
  ghclass_api_v3_req(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = path,
    content = base64enc::base64encode(content),
    message = message, 
    branch = branch,
    sha = sha
  )
}

#' @rdname repo_file
#'
#' @param content Character or raw. Content of the file.
#' @param verbose Logical. Should success / failure messages be printed
#'
#' @export
#'
repo_put_file = function(repo, path, content, message = NULL, branch = "master", verbose = TRUE) {

  arg_is_chr_scalar(repo, path, branch)
  arg_is_chr_scalar(message, allow_null = TRUE)

  if (is.null(message))
    message = cli_glue("Adding file {path}")

  if (is.character(content))
    content = charToRaw(content)

  # To update an existing file we need its current SHA,
  # if the file does not exist this will be NULL.
  cur_file = repo_get_file(repo, path, branch, quiet=TRUE)
  sha = attr(cur_file, "sha")

  res = purrr::safely(github_api_repo_put_file)(repo, path, content, message, branch, sha)

  if(verbose){
    status_msg(
      res,
      "Added file {.val {path}} to repo {.val {repo}}.",
      "Failed to add file {.val {path}} to repo {.val {repo}}."
    )
  }

  res
}




peer_github_api_repo_put_file = function(repo, path, content, message, branch, sha) {

  args = list(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = path,
    content = base64enc::base64encode(content),
    message = message, branch = branch
  )

  if (!is.null(sha)) {
    args[["sha"]] = sha
  }

  do.call(ghclass_api_v3_req, args)
}



peer_repo_put_file = function(repo, path, content, message = NULL, branch = "master", sha, verbose = TRUE) {
  arg_is_chr_scalar(repo, path, branch)
  arg_is_chr_scalar(message, sha, allow_null = TRUE)

  if (is.null(message))
    message = cli_glue("Adding file: {path}")

  if (is.character(content))
    content = charToRaw(content)

  res = purrr::safely(peer_github_api_repo_put_file)(
    repo = repo, path = path, content = content,
    message = message, branch = branch, sha = sha
  )

  if (verbose){
    status_msg(
      res,
      "Added file {.val {path}} to repo {.val {repo}}.",
      "Failed to add file {.val {path}} to repo {.val {repo}}."
    )
  }

  res
}
