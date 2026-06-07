#github_repo_pattern ="^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+)/([A-Za-z0-9_.-]+)$"
#github_username_pattern = "^[A-Za-z\\d](?:[A-Za-z\\d]|-(?=[A-Za-z\\d])){0,38}$"

# Use a simplified pattern and let GitHub sort out the particulars
github_repo_pattern ="^([A-Za-z0-9-]+)/([A-Za-z0-9_.-]+)$"


valid_repo_error = function(repo) {
  cli_stop( paste(
    "Invalid repository name(s) {.val {repo}}.",
    "Repository names must be in {.val 'owner/name'} format."
  ) )
}


#' @rdname ghclass-internal
#' @export
match_repo = function(repo, index=1) {
  arg_is_chr(repo)

  m = regexec(github_repo_pattern, repo)
  m = regmatches(repo, m)

  l = purrr::map_int(m, length)

  if (any(l != 3))
    valid_repo_error(repo[l != 3])

  purrr::map_chr(m, index)
}


#' @rdname ghclass-internal
#' @export
get_repo_name = function(repo) {
  match_repo(repo, 3)
}

#' @rdname ghclass-internal
#' @export
get_repo_owner = function(repo) {
  match_repo(repo, 2)
}

#' @rdname ghclass-internal
#' @export
format_repo = function(repo, branch = NULL, path = NULL) {
  if (!is.null(branch)) {
    repo = paste(repo, branch, sep="@")
  }

  if (!is.null(path))
    repo = file.path(repo, path)

  repo
}
