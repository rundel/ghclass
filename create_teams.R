#!/usr/bin/env Rscript

library(gh)
library(magrittr)

args = commandArgs(trailingOnly=TRUE)


if(length(args)!=3)
{
  cat("Usage: invite_accounts.R <account file> <organization> <prefix>\n")
  stop()
}

account_file = args[1]
org = args[2]
prefix = args[3]

stopifnot(file.exists(account_file))
team_info = read.csv(account_file, stringsAsFactors=FALSE)

stopifnot(all(c("Name","Account","Team") %in% names(team_info)))
team_info$Team = paste0(prefix, team_info$Team)
teams = team_info$Team %>%
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

teams = gh("/orgs/:org/teams", org=org, .token=token)
team_ids = sapply(teams, function(x) x$id)
names(team_ids) = sapply(teams, function(x) x$name)

for(i in seq_len(nrow(team_info)))
{
    Sys.sleep(0.2)

    team = team_info$Team[i]
    acc = team_info$Account[i]
    id = team_ids[team]

    cat("Adding ", acc, " to ", team, "...\n", sep="")

    gh("PUT /teams/:id/memberships/:username", 
       id=id, username=acc,
       role="member",
       .token=token)
}
