#!/usr/bin/env Rscript

suppressMessages(library(gh))
suppressMessages(library(magrittr))
suppressMessages(library(stringr))
suppressMessages(library(git2r))

args = commandArgs(trailingOnly=TRUE)
token = readLines("secret/github_token")

if(length(args) < 4)
{
  cat("Usage: add_files.R <organization> <repo suffix> <message> <file1> <file2> ...")
  stop()
}

org = args[1]
suffix = args[2]
message = args[3]
files = args[4:length(args)]


repos = gh("GET /orgs/:org/repos", org = org, .token=token)
repo_names = sapply(repos, function(x) x$name)   

selected_repos = str_detect(repo_names,suffix) %>% repo_names[.]


for(repo in repo_names)
{
  cat("Updating", repo, "...\n")
  org_url = paste0('git@github.com:',org,'/',repo,'.git')

  path = file.path(tempdir(),repo)
  dir.create(path, recursive=TRUE)

  local_repo = clone(org_url, path, progress=FALSE)  
  for(file in files)
  {
    file.copy(file, path)
    add(local_repo, basename(file))
  }

  commit(local_repo, message)
  push(local_repo)

  unlink(path, recursive=TRUE)
}

