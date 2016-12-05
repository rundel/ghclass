#!/usr/bin/env Rscript

suppressMessages(library(gh))
suppressMessages(library(magrittr))
suppressMessages(library(stringr))
suppressMessages(library(git2r))

args = commandArgs(trailingOnly=TRUE)
token = readLines("secret/github_token")

if (!length(args) %in% c(3,4))
{
  cat("Usage: grab_repos.R <organization> <repo pattern> <local path> [progress=FALSE]\n")
  stop()
}

org = args[1]
pattern = args[2]
local_path = args[3]

progress = FALSE
if (length(args) == 4)
  progress = as.logical(args[4])

repos = gh("GET /orgs/:org/repos", org = org, .token=token, .limit=1000)
repo_names = sapply(repos, function(x) x$name)   

selected_repos = str_detect(repo_names,pattern) %>% repo_names[.]


for(repo in selected_repos)
{
  cat("Grabbing", repo, "...\n")
  org_url = paste0('git@github.com:',org,'/',repo,'.git')

  d = file.path(local_path,repo)
  dir.create(d, recursive=TRUE, showWarnings=FALSE)

  local_repo = clone(org_url, d, progress=progress)  
}

