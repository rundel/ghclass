#!/usr/bin/env Rscript

library(gh)
library(magrittr)

args = commandArgs(trailingOnly=TRUE)


if(length(args)!=3)
{
  cat("Usage: invite_accounts.R ACCOUNT_FILE ORGANIZATION PREFIX")
  stop()
}

account_file = args[1]
org = args[2]
prefix = args[3]

stopifnot(file.exists(account_file))

account_info = read.csv(account_file, stringsAsFactors=FALSE)

account_info$Team = paste0(prefix, account_info$Team)

teams = account_info$Team %>%
        unique() %>%
        sort()
        

token = readLines("secret/github_token")


for(team in teams)
{
  Sys.sleep(0.2)

  cat("Adding ", team, "...\n", sep="")
  gh("POST /orgs/:org/teams", 
     org=org, 
     name=team, privacy="closed",
     .token=token)
}

team_info = gh("/orgs/:org/teams", org=org, .token=token)
team_ids = sapply(team_info, function(x) x$id)
names(team_ids) = sapply(team_info, function(x) x$name)

for(i in seq_len(nrow(account_info)))
{
    Sys.sleep(0.2)

    team = account_info$Team[i]
    acc = account_info$Account[i]
    id = team_ids[team]

    cat("Adding ", acc, " to ", team, "...\n", sep="")

    gh("PUT /teams/:id/memberships/:username", 
       id=id, username=acc,
       role="member",
       .token=token)
}
