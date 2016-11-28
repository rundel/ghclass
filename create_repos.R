#!/usr/bin/env Rscript

library(gh)
library(magrittr)

args = commandArgs(trailingOnly=TRUE)

token = readLines("secret/github_token")

if(!length(args) %in% 2:3)
{
  cat("Usage: create_repos.R <organization> <suffix> [public repo]\n")
  stop()
}

org = args[1]
suffix = args[2]
public = NULL
if (length(args)==3)
{
  repo_name = paste0(args[3], suffix)
  cat("Creating", repo_name, "\n")
  
  try({
    x = gh("POST /orgs/:org/repos", 
           org = org,
           name=repo_name, private=FALSE,
           auto_init=TRUE, gitignore_template="R",
           .token=token) 
  })
}
        

team_info = gh("/orgs/:org/teams", org=org, .token=token, .limit=1000)
team_ids = sapply(team_info, function(x) x$id)
names(team_ids) = sapply(team_info, function(x) x$name)

for(team in names(team_ids))
{
    Sys.sleep(1)

    repo_name = paste0(team,suffix)
    cat("Creating ", repo_name, " for ",team," (", team_ids[team],")\n",sep="")

    try({
      gh("POST /orgs/:org/repos", 
         org = org,
         name=repo_name, private=TRUE, team_id=team_ids[team], 
         auto_init=TRUE, gitignore_template="R",
         .token=token)
    })

    Sys.sleep(2)

    try({
      gh("PUT /teams/:id/repos/:org/:repo", 
         id = team_ids[team], org = org, repo = repo_name,
         permission="push",
         .token=token)
    })
}


