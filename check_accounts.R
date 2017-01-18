#!/usr/bin/env Rscript

library(gh)

args = commandArgs(trailingOnly=TRUE)

if(length(args) != 2)
{
  cat("Usage: check_accounts.R <account file> <account column>\n")
  stop()
}

account_file = args[1]
account_col  = args[2]

stopifnot(file.exists(account_file))

token = readLines("secret/github_token")

stopifnot(file.exists(account_file))
account_info = read.csv(account_file, stringsAsFactors=FALSE)

stopifnot(account_col %in% names(account_info))
accounts = account_info[[account_col]]

res = lapply(
  accounts, 
  function(acc)
  {
    Sys.sleep(0.2)
    try( gh("/users/:username", username=acc, .token=token), silent=TRUE)
  } 
)

invalid = sapply(res, function(x) any(class(x) == "try-error"))

if (sum(invalid) > 0) {
  cat("Invalid accounts:", accounts[invalid], "\n")
} else {
  cat("All accounts are valid!\n")
}
