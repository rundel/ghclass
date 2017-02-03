clean_repo_names = function(repo_names)
{
  repo_names %>%
    str_replace(" ", "_") %>%
    str_replace("__", "_")
}

check_repos = function(repos)
{
  #users %>%
  #  clean_usernames() %>%
  #  map(~ try( {gh("GET /repos/:owner/:repo", username=., .token=get_github_token())}, silent=TRUE)) %>%
  #  map_lgl(~ !any(class(.) == "try-error"))
}


create_repos = function(org, teams=NULL, prefix="", suffix="", verbose=TRUE)
{
  if (is.null(teams))
    teams = get_org_teams(org)

  if (prefix == "" & suffix == "")
    stop("Either prefix or suffix must be specified")

  if (prefix != "" & !str_detect(prefix,"_$"))
    prefix = paste0(prefix,"_")

  if (suffix != "" & !str_detect(suffix,"^_"))
    suffix = paste0("_",suffix)


  for(team in names(teams))
  {
    repo_name = paste0(prefix, team, suffix) %>% clean_repo_names()

    if (verbose)
      cat("Creating ", repo_name, " for ",team," (", teams[team],")\n",sep="")

    try({
      gh("POST /orgs/:org/repos",
         org = org,
         name=repo_name, private=TRUE, team_id=teams[team],
         auto_init=TRUE, gitignore_template="R",
         .token=get_github_token())
    })

    Sys.sleep(0.5)

    try({
      gh("PUT /teams/:id/repos/:org/:repo",
         id = teams[team], org = org, repo = repo_name,
         permission="push",
         .token=get_github_token())
    })
  }
}


#add_badge = function(org, pattern, badge, verbose=TRUE, url_type = c("ssh","https"))
#{
#  stopifnot(length(pattern) == 1)
#  stopifnot(length(badge) == 1)
#
#  url_type = match.arg(url_type)
#
#  repos = get_org_repos(org, pattern)
#
#
#  for(repo in repo)
#  {
#    if (verbose)
#      cat("Adding badge for", repo, "...\n")
#
#    if (url_type == "ssh")
#      org_url = paste0('git@github.com:',org,'/',repo,'.git')
#    else
#      org_url = paste0("https://github.com/",org,"/",repo,".git")
#
#    path = file.path(tempdir(),repo)
#    dir.create(path, recursive=TRUE)
#
#    local_repo = clone(org_url, path, progress=FALSE)
#
#    readme = file.path(path,"README.md")
#
#    try({
#      stopifnot(file.exists(readme))
#
#      prev_contents = readLines(readme, warn=FALSE)
#      writeLines(
#        c(badge, prev_contents),
#        readme
#      )
#
#      add(local_repo, readme)
#      commit(local_repo, "Added badge")
#      push(local_repo)
#    })
#
#    unlink(path, recursive=TRUE)
#  }
#}

add_files = function(org, pattern=NULL, message, files, branch = "master", preserve_path=FALSE, verbose=TRUE)
{
  stopifnot(all(file.exists(files)))

  repos = get_org_repos(org, pattern)

  for(repo in repos)
  {
    if (verbose)
      cat("Adding files to", repo, "...\n")


    for(file in files)
    {
      gh_path = file
      if (!preserve_path)
        gh_path = basename(file)

      gh_file = try({
        gh("GET /repos/:owner/:repo/contents/:path",
          owner = org, repo = repo, path=gh_path,
          ref = branch,
         .token=get_github_token(), .limit=get_api_limit())
      }, silent = TRUE)

      content = base64enc::base64encode(file)

      tryCatch(
        {
          if ("try-error" %in% class(gh_file)) # File does not exist
          {
            gh("PUT /repos/:owner/:repo/contents/:path",
               owner = org, repo = repo, path=gh_path,
               message = message, content = content, branch = branch)
          } else { # File already exists
            gh("PUT /repos/:owner/:repo/contents/:path",
               owner = org, repo = repo, path=gh_path,
               message = message, content = content, branch = branch,
               sha = gh_file$sha)
          }
        },
        error = function(e)
          message("Adding ", file, " to ", repo, "failed.")
      )
    }
  }
}

repo_url = function(repo, type = c("https","ssh"), use_token = TRUE)
{
  type = match.arg(type)

  if (type == "https")
  {
    if (use_token)
      url = paste0("https://", get_github_token(), "@github.com/",repo,".git")
    else
      url = paste0("https://github.com/",repo,".git")
  } else {
    url = paste0("git@github.com:",repo,".git")
  }

  return(url)
}

copy_contents = function(source_repo, target_repos, verbose=TRUE)
{
  stopifnot(!missing(source_repo))
  stopifnot(!missing(target_repos))
  stopifnot(length(source_repo) == 1)

  if (Sys.which("git") == "")
    stop("git must be installed for this function to work")

  cur_wd = getwd()
  setwd(tempdir())

  if (verbose)
    cat("Cloning source repo ...\n")

  system(paste0("git clone --bare ", repo_url(source_repo)), intern = FALSE,
         wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  git_dir = dir(pattern = "\\.git")
  stopifnot(length(git_dir) == 1)

  setwd(git_dir)

  for(repo in target_repos)
  {
    if (verbose)
      cat("Mirroring to", repo," ...\n")

    system(paste0("git push --mirror ", repo_url(repo)), intern = FALSE,
           wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  setwd("..")

  if (verbose)
    cat("Cleaning up ...\n")

  unlink(git_dir, recursive = TRUE)
  setwd(cur_wd)
}


