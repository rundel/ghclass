github_repo_pattern ="^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+)/([A-Za-z0-9_.-]+)$"
github_username_pattern = "^[A-Za-z\\d](?:[A-Za-z\\d]|-(?=[A-Za-z\\d])){0,38}$"


clean_usernames = function(usernames)
{
  s = stringr::str_trim(usernames)
  s[s != ""]
}

# Should only be used within GitHub API calls,
# i.e. functions starting with github_api_*
# Explicitly not vectorized
require_valid_repo = function(repo)
{
  stopifnot(length(repo) == 1)

  if (!valid_repo(repo))
    usethis::ui_stop(paste(
      "Invalid repository name {usethis::ui_value(repo)}.",
      "Repository names must be in {usethis::ui_code('owner/name')} format."
    ) )
}

#' @export
valid_repo = function(repo)
{
  stringr::str_detect(repo, github_repo_pattern)
}

#' @export
get_repo_name = function(repo)
{
  stringr::str_match(repo, github_repo_pattern)[,3]
}

#' @export
get_repo_owner = function(repo)
{
  stringr::str_match(repo, github_repo_pattern)[,2]
}

#' @export
get_repo_url = function(repo, type = c("https","ssh"), use_token = TRUE)
{
  #TO DO: Fix since require_valid_repo is no longer vectorized
  #require_valid_repo(repo)
  type = match.arg(type)

  if (type == "https") {
    if (use_token)
      paste0("https://", get_github_token(), "@github.com/",repo,".git")
    else
      paste0("https://github.com/",repo,".git")
  } else {
    paste0("git@github.com:",repo,".git")
  }
}



format_repo = function(repo, branch = "master", file = NULL) {
  repo = if (branch == "master") {
    repo
  } else{
    paste(repo, branch, sep="@")
  }

  if (!is.null(file))
    repo = file.path(repo, file)

  repo
}

