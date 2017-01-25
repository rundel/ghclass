clean_repo_names = function(repo_names)
{
  repo_names %>%
    str_replace(" ", "_") %>%
    str_replace("__", "_")
}

create_repos = function(org, teams=get_org_teams(org), prefix="", suffix="", verbose=TRUE)
{
  if (prefix != "" & str_detect(prefix,"_$"))
    prefix = paste0(prefix,"_")

  if (suffix != "" & str_detect(suffix,"^_"))
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
         .token=token)
    })

    Sys.sleep(0.5)

    try({
      gh("PUT /teams/:id/repos/:org/:repo",
         id = teams[team], org = org, repo = repo_name,
         permission="push",
         .token=token)
    })
  }
}


add_badge = function(org, pattern, badge, verbose=TRUE, url_type = c("ssh","https"))
{
  stopifnot(length(pattern) == 1)
  stopifnot(length(badge) == 1)

  url_type = match.arg(url_type)

  repos = get_org_repos(org, pattern)


  for(repo in repo)
  {
    if (verbose)
      cat("Adding badge for", repo, "...\n")

    if (url_type == "ssh")
      org_url = paste0('git@github.com:',org,'/',repo,'.git')
    else
      org_url = paste0("https://github.com/",org,"/",repo,".git")

    path = file.path(tempdir(),repo)
    dir.create(path, recursive=TRUE)

    local_repo = clone(org_url, path, progress=FALSE)

    readme = file.path(path,"README.md")

    try({
      stopifnot(file.exists(readme))

      prev_contents = readLines(readme, warn=FALSE)
      writeLines(
        c(badge, prev_contents),
        readme
      )

      add(local_repo, readme)
      commit(local_repo, "Added badge")
      push(local_repo)
    })

    unlink(path, recursive=TRUE)
  }
}

add_files = function(org, pattern, message, files)
{
  # repos = gh("GET /orgs/:org/repos", org = org, .token=token, .limit=1000)
  # repo_names = sapply(repos, function(x) x$name)
  #
  # selected_repos = str_detect(repo_names, pattern) %>% repo_names[.]
  #
  # for(repo in selected_repos)
  # {
  #   cat("Updating", repo, "...\n")
  #   org_url = paste0('git@github.com:',org,'/',repo,'.git')
  #   #org_url = paste0("https://github.com/",org,"/",repo,".git")
  #
  #   path = file.path(tempdir(),repo)
  #   dir.create(path, recursive=TRUE)
  #
  #   local_repo = clone(org_url, path, progress=FALSE)#, credentials = cred)
  #   try({
  #     for(file in files)
  #     {
  #       file.copy(file, path, overwrite = TRUE)
  #       add(local_repo, basename(file))
  #     }
  #
  #     commit(local_repo, message)
  #     push(local_repo)#, credentials = cred)
  #   })
  #
  #   unlink(path, recursive=TRUE)
  # }
}





