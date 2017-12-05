github_repo_pattern ="^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+)/([A-Za-z0-9_.-]+)$"
github_username_pattern = "^[A-Za-z\\d](?:[A-Za-z\\d]|-(?=[A-Za-z\\d])){0,38}$"


clean_usernames = function(usernames)
{
  s = str_trim(usernames)
  s[s != ""]
}

require_valid_repo = function(repos)
{
  valid = valid_repo(repos)
  if (!all(valid))
    stop("Invalid repo names: \n\t", paste(repos[!valid], collapse="\n\t"))
}

#' @export
valid_repo = function(repos, require_owner=TRUE)
{
  str_detect(repos, github_repo_pattern)
}

#' @export
get_repo_name = function(repos)
{
  str_match(repos, github_repo_pattern)[,3]
}

#' @export
get_repo_owner = function(repos)
{
  str_match(repos, github_repo_pattern)[,2]
}

#' @export
get_repo_url = function(repos, type = c("https","ssh"), use_token = TRUE)
{
  type = match.arg(type)

  stopifnot(all(valid_repo(repos, require_owner = TRUE)))

  if (type == "https")
  {
    if (use_token)
      urls = paste0("https://", get_github_token(), "@github.com/",repos,".git")
    else
      urls = paste0("https://github.com/",repos,".git")
  } else {
    urls = paste0("git@github.com:",repos,".git")
  }

  return(urls)
}
