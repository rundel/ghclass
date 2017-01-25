#!/usr/bin/env Rscript

library(gh)
library(purrr)
library(stringr)

args = commandArgs(trailingOnly=TRUE)


if(length(args) != 1)
{
  cat("Usage: list_accounts.R <organization> \n")
  stop()
}

token = readLines("secret/github_token")

org = args[1]

members = map_chr(gh("GET /orgs/:org/members", org=org, .token=token, .limit=1000), "login")

cat(members, sep="\n")