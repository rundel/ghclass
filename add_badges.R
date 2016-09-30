#!/usr/bin/env Rscript

suppressMessages(library(gh))
suppressMessages(library(magrittr))
suppressMessages(library(stringr))
suppressMessages(library(git2r))
suppressMessages(library(dplyr))

args = commandArgs(trailingOnly=TRUE)
token = readLines("secret/github_token")

if(length(args) != 3)
{
  cat("Usage: add_badges.R <organization> <repo suffix> <badge file>\n")
  stop()
}

org = args[1]
suffix = args[2]
badges = read.csv(args[3], stringsAsFactors=FALSE, quote="")

stopifnot(all(names(badges) == c("team","link")))


repos = gh("GET /orgs/:org/repos", org = org, .token=token, .limit=1000)
repo_names = sapply(repos, function(x) x$name)   
selected_repos = str_detect(repo_names,suffix) %>% repo_names[.]


cat("Username: ")
user = readLines(file("stdin"),1)
cat("Password: ")
pass = readLines(file("stdin"),1)

cred = cred_user_pass(user,pass)

for(repo in selected_repos)
{
  team = str_replace(repo, suffix, "")
  link = badges$link[badges$team == team]

  stopifnot(length(link) == 1)

  cat("Adding badge for", repo, "...\n")
  #org_url = paste0('git@github.com:',org,'/',repo,'.git')
  org_url = paste0("https://github.com/",org,"/",repo,".git")

  path = file.path(tempdir(),repo)
  dir.create(path, recursive=TRUE)

  local_repo = clone(org_url, path, progress=FALSE, credentials = cred)

  readme = file.path(path,"README.md")
  stopifnot(file.exists(readme))

  try({
    prev_contents = readLines(readme, warn=FALSE)
    writeLines(
      c(link, prev_contents),
      readme
    )
  
    add(local_repo, readme)
    commit(local_repo, "Adding badge")
    push(local_repo, credentials = cred)
  })
  
  unlink(path, recursive=TRUE)
}

