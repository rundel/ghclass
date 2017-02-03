require_valid_repo = function(repos, require_owner)
{
  valid = valid_repo(repos, requier_owner = require_owner)
  if (!all(valid))
  {
    stop("Invalid repo names: \n\t", paste(repos[!valid], collapse="\n\t"))
  }
}

valid_repo = function(repos, requier_owner=TRUE)
{
  if (requier_owner)
    str_detect(repos, "^[A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+/[A-Za-z0-9_.-]+$")
  else
    str_detect(repos, "^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+/)?[A-Za-z0-9_.-]+$")
}

get_repo_name = function(repos)
{
  res = str_match(repos, "^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+/)?([A-Za-z0-9_.-]+)$")
  res = res[,3]
  res
}

get_repo_owner = function(repos)
{
  res = str_match(repos, "^([A-Za-z0-9]+[A-Za-z0-9-]*[A-Za-z0-9]+)/[A-Za-z0-9_.-]+$")
  res = res[,2]
  res
}


repo_url = function(repos, type = c("https","ssh"), use_token = TRUE)
{
  type = match.arg(type)

  stopifnot(all(valid_repo(repos, requier_owner = TRUE)))

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
