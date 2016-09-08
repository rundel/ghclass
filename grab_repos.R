#!/usr/bin/env Rscript

suppressMessages(library(gh))
suppressMessages(library(magrittr))
suppressMessages(library(stringr))
suppressMessages(library(git2r))

args = commandArgs(trailingOnly=TRUE)
token = readLines("secret/github_token")

if(length(args) != 3)
{
  cat("Usage: grab_repos.R <organization> <repo pattern> <local path>")
  stop()
}

org = args[1]
pattern = args[2]
local_path = args[3]

repos = gh("GET /orgs/:org/repos", org = org, .token=token)
repo_names = sapply(repos, function(x) x$name)   

selected_repos = str_detect(repo_names,pattern) %>% repo_names[.]


for(repo in repo_names)
{
  cat("Grabbing", repo, "...\n")
  org_url = paste0('git@github.com:',org,'/',repo,'.git')

  d = file.path(local_path,repo)
  dir.create(d, recursive=TRUE)

  local_repo = clone(org_url, d, progress=FALSE)  
}

