#!/usr/bin/env Rscript

library(magrittr)
library(knitr)
library(rmarkdown)

args = commandArgs(trailingOnly=TRUE)

if(length(args) != 1)
{
  cat("Usage: knit_rmds.R <local repos dir>\n")
  stop()
}

local_path = args[1]

stopifnot(dir.exists(local_path))

repos = list.dirs(path = local_path, full.names = TRUE, recursive = FALSE)      

prev_dir = getwd()

for(repo in repos)
{
  cat("Knitting in", repo, "...\n")

  setwd(repo)
  rmds = list.files("./", pattern="[Rr]md$")
  lapply(rmds, render, quiet=TRUE)

  setwd(prev_dir)
}