#!/usr/bin/env Rscript

library(gh)

acc_file = commandArgs(trailingOnly=TRUE)

stopifnot(length(acc_file) == 1)
stopifnot(file.exists(acc_file))

token = readLines("secret/github_token")
accounts = readLines(acc_file)

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
