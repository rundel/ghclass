#!/usr/bin/env Rscript

suppressMessages(library(gh))
suppressMessages(library(magrittr))
suppressMessages(library(stringr))
suppressMessages(library(git2r))

args = commandArgs(trailingOnly=TRUE)
token = readLines("secret/github_token")

if(length(args) < 4)
{
  cat("Usage: add_files.R <organization> <repo pattern> <message> <file1> <file2> ...\n")
  stop()
}

org = args[1]
pattern = args[2]
message = args[3]
files = args[4:length(args)]


repos = gh("GET /orgs/:org/repos", org = org, .token=token, .limit=1000)
repo_names = sapply(repos, function(x) x$name)   

selected_repos = str_detect(repo_names, pattern) %>% repo_names[.]

for(repo in selected_repos)
{
  cat("Updating", repo, "...\n")
  org_url = paste0('git@github.com:',org,'/',repo,'.git')
  #org_url = paste0("https://github.com/",org,"/",repo,".git")

  path = file.path(tempdir(),repo)
  dir.create(path, recursive=TRUE)

  local_repo = clone(org_url, path, progress=FALSE)#, credentials = cred)
  try({
    for(file in files)
    {
      file.copy(file, path, overwrite = TRUE)
      add(local_repo, basename(file))
    }

    commit(local_repo, message)
    push(local_repo)#, credentials = cred)
  })
  
  unlink(path, recursive=TRUE)
}

