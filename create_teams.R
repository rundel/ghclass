#!/usr/bin/env Rscript

library(gh)
library(magrittr)

args = commandArgs(trailingOnly=TRUE)


if(length(args)!=4)
{
  cat("Usage: create_teams.R <organization> <account file> <account column> <team column>\n")
  stop()
}

token = readLines("secret/github_token")

org = args[1]
account_file = args[2]
account_col = args[3]
team_col = args[4]

stopifnot(file.exists(account_file))
team_info = read.csv(account_file, stringsAsFactors=FALSE)

stopifnot(all(c(account_col, team_col) %in% names(team_info)))
stopifnot(all(team_info[[team_col]] != ""))

teams = team_info[[team_col]] %>%
        unique() %>%
        sort()
        

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

    team = team_info[[team_col]][i]
    acc = team_info[[account_col]][i]
    id = team_ids[team]

    if (acc == "")
      next

    cat("Adding ", acc, " to ", team, "...\n", sep="")

    gh("PUT /teams/:id/memberships/:username", 
       id=id, username=acc,
       role="member",
       .token=token)
}
