#github_repo_pattern ="^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+)/([A-Za-z0-9_.-]+)$"
#github_username_pattern = "^[A-Za-z\\d](?:[A-Za-z\\d]|-(?=[A-Za-z\\d])){0,38}$"

# Use a simplified pattern and let GitHub sort out the particulars
github_repo_pattern ="^([A-Za-z0-9-]+)/([A-Za-z0-9_.-]+)$"


valid_repo_error = function(repo) {
  cli_stop(
    "Invalid repository name(s) {.val {repo}}.",
    "Repository names must be in {.val 'owner/name'} format."
  )
}

match_repo = function(repo, index=1) {
  arg_is_chr(repo)

  m = regexec(github_repo_pattern, repo)
  m = regmatches(repo, m)

  l = purrr::map_int(m, length)

  if (any(l != 3))
    valid_repo_error(repo[l != 3])

  purrr::map_chr(m, index)
}


get_repo_name = function(repo) {
  match_repo(repo, 3)
}

get_repo_owner = function(repo) {
  match_repo(repo, 2)
}

get_repo_url = function(repo, type = c("https","ssh"), use_token = TRUE)
{
  #TO DO: Fix since require_valid_repo is no longer vectorized
  #require_valid_repo(repo)
  type = match.arg(type)

  if (type == "https") {
    if (use_token)
      paste0("https://", github_get_token(), "@github.com/",repo,".git")
    else
      paste0("https://github.com/",repo,".git")
  } else {
    paste0("git@github.com:",repo,".git")
  }
}


format_repo = function(repo, branch = "master", path = NULL) {
  repo = if (is.null(branch) || branch == "master") {
    repo
  } else{
    paste(repo, branch, sep="@")
  }

  if (!is.null(path))
    repo = file.path(repo, path)

  repo
}

