#!/usr/bin/env Rscript

suppressMessages(library(git2r))

args = commandArgs(trailingOnly=TRUE)
token = readLines("secret/github_token")

if (!length(args) == 1)
{
  cat("Usage: update_repos.R <local path>\n")
  stop()
}

local_path = args[1]

repos = list.dirs(path = local_path, full.names = TRUE, recursive=FALSE)
print(repos)

for(repo in repos)
{
  cat("Updating", repo, "...")
  pull(repo = repository(repo))
}

