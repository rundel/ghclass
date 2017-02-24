create_team_repos = function(org, teams = get_org_teams(org), prefix="", suffix="", verbose=TRUE, delay=0.2)
{
  if (prefix == "" & suffix == "")
    stop("Either a prefix or a suffix must be specified")

  if (prefix != "" & !str_detect(prefix,"_$"))
    prefix = paste0(prefix,"_")

  if (suffix != "" & !str_detect(suffix,"^_"))
    suffix = paste0("_",suffix)

  if (is.character(teams))
  {
    org_teams = get_org_teams(org)
    teams = org_teams[ teams %in% names(org_teams) ]
  }

  for(team in names(teams))
  {
    repo_name = paste0(prefix, team, suffix) %>%
        str_replace_all(" ", "_") %>%
        str_replace_all("_+", "_")

    if (verbose)
      cat("Creating ", repo_name, " for ",team," (", teams[team],")\n",sep="")

    try({
      gh("POST /orgs/:org/repos",
         org = org,
         name=repo_name, private=TRUE, team_id=teams[team],
         auto_init=TRUE, gitignore_template="R",
         .token=get_github_token())
    })

    Sys.sleep(delay)

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

add_files = function(repos, message, files, branch = "master", preserve_path=FALSE, verbose=TRUE)
{
  stopifnot(all(file.exists(files)))
  stopifnot(all(valid_repo(repos, require_owner = TRUE)))

  repo_name  = get_repo_name(repos)
  repo_owner = get_repo_owner(repos)

  for(i in seq_along(repos))
  {
    repo = repo_name[i]
    owner = repo_owner[i]

    if (verbose)
      cat("Adding files to", repos[i], "...\n")

    for(file in files)
    {
      gh_path = file
      if (!preserve_path)
        gh_path = basename(file)

      gh_file = try({
        gh("GET /repos/:owner/:repo/contents/:path",
          owner = owner, repo = repo, path=gh_path,
          ref = branch,
         .token=get_github_token(), .limit=get_api_limit())
      }, silent = TRUE)

      content = base64enc::base64encode(file)

      tryCatch(
        {
          if ("try-error" %in% class(gh_file)) # File does not exist
          {
            gh("PUT /repos/:owner/:repo/contents/:path",
               owner = owner, repo = repo, path=gh_path,
               message = message, content = content, branch = branch,
               .token=get_github_token())
          } else { # File already exists
            gh("PUT /repos/:owner/:repo/contents/:path",
               owner = owner, repo = repo, path=gh_path,
               message = message, content = content, branch = branch,
               sha = gh_file$sha, .token=get_github_token())
          }
        },
        error = function(e)
          message("Adding ", file, " to ", repo, "failed.")
      )
    }
  }
}

grab_repos = function(repos, localpath="./", verbose=TRUE)
{
  stopifnot(!missing(repos))

  git = require_git()

  cur_dir = getwd()
  setwd(localpath)



  for (repo in repos)
  {
    if (verbose)
      cat("Cloning ", repo, "\n")

    system(
      paste0(git, " clone ", repo_url(repo)),
      intern = FALSE, wait = TRUE, ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
  }

  setwd(cur_dir)
}




mirror_repo = function(source_repo, target_repos, verbose=TRUE)
{
  stopifnot(!missing(source_repo))
  stopifnot(!missing(target_repos))
  stopifnot(length(source_repo) == 1)

  git = require_git()

  cur_dir = getwd()
  setwd(tempdir())

  if (verbose)
    cat("Cloning source repo ...\n")

  system(paste0(git, " clone --bare ", repo_url(source_repo)), intern = FALSE,
         wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  repo_dir = dir(pattern = "\\.git")
  stopifnot(length(repo_dir) == 1)

  setwd(repo_dir)

  for(repo in target_repos)
  {
    if (verbose)
      cat("Mirroring to", repo," ...\n")

    system(paste0(git, " push --mirror ", repo_url(repo)), intern = FALSE,
           wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  setwd("..")

  if (verbose)
    cat("Cleaning up ...\n")

  unlink(repo_dir, recursive = TRUE)
  setwd(cur_dir)
}


