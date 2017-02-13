#!/usr/bin/env Rscript

library(gh)
library(purrr)
library(stringr)

args = commandArgs(trailingOnly=TRUE)


if(length(args) != 3)
{
  cat("Usage: invite_accounts.R <organization> <account file> <account column> \n")
  stop()
}

token = readLines("secret/github_token")

org = args[1]
account_file = args[2]
account_col  = args[3]


stopifnot(file.exists(account_file))

file = read.csv(account_file, stringsAsFactors=FALSE)

stopifnot(account_col %in% names(file))
accounts = str_trim(file[[account_col]])

accounts = accounts[accounts != ""]


members = map_chr(gh("GET /orgs/:org/members", org=org, .token=token, .limit=1000), "login")

need_invite = setdiff(tolower(accounts), tolower(members))

for(acc in need_invite)
{
  Sys.sleep(0.2)

  cat("Adding ", acc, " ...\n", sep="")
  gh("PUT /orgs/:org/memberships/:username", 
     org=org, username=acc, role="member",
     .token=token)
}
