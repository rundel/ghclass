#!/usr/bin/env Rscript

library(gh)

args = commandArgs(trailingOnly=TRUE)



if(length(args)!=2)
{
  cat("Usage: invite_accounts.R <account file> <organization>")
  stop()
}

account_file = args[1]
org = args[2]

stopifnot(file.exists(account_file))

accounts = read.csv(account_file, stringsAsFactors=FALSE)$Account
token = readLines("secret/github_token")


for(acc in accounts)
{
  Sys.sleep(0.2)

  cat("Adding ", acc, "...\n", sep="")
  gh("PUT /orgs/:org/memberships/:username", 
     org=org, username=acc, role="member",
     .token=token)
}
