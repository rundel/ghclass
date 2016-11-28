#!/usr/bin/env Rscript

library(gh)
library(magrittr)
library(stringr)

args = commandArgs(trailingOnly=TRUE)

token = readLines("secret/github_token")

if(!length(args) %in% 2:3)
{
  cat("Usage: list_repos.R <organization> <pattern>\n")
  stop()
}

org = args[1]
pattern = args[2]

        
repos = gh("GET /orgs/:org/repos", org = org, .token=token, .limit=1000)
repo_names = sapply(repos, function(x) x$name)   

str_detect(repo_names,pattern) %>% repo_names[.] %>% paste(collapse="\n") %>% cat("\n")
