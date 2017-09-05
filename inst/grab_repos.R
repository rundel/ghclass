#!/usr/bin/env Rscript

suppressMessages(library(ghclass))
suppressMessages(library(optparse))
suppressMessages(library(stringr))

options = list(
  make_option(c("-o", "--org"), type = "character", default=NULL,
              help="Organization that owns the repos"),
  make_option(c("-f", "--filter"), type = "character", default=NULL,
              help="Filter to select repos"),
  make_option(c("-x", "--exclude"), action = "store_true", default=FALSE,
              help="Exclude filter matches"),
  make_option(c("-r", "--repos"), type = "character", default=NULL,
              help="Comma separated list of repos in the owner/repo format."),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Print extra output [default]"),
  make_option(c("-p", "--path"), type="character", default="./",
              help="Local path to save repos in"),
  make_option(c("--dryrun"), action="store_true", default=FALSE,
              help="Don't actually grab repos, describe what would happen when run.")
)


parser = OptionParser(usage = "%prog [options]", option_list=options)

cmd = parse_args(parser, positional_arguments = FALSE)

repos = c()
if (!is.null(cmd$org))
  repos = get_repos(cmd$org, cmd$filter, cmd$exclude, full_repo=TRUE)

if (!is.null(cmd$repos))
  repos = c(repos, str_split(cmd$repos,",")[[1]])


require_valid_repo(repos, require_owner=TRUE)

if (!dir.exists(cmd$path))
  dir.create(cmd$path, showWarnings = FALSE, recursive = TRUE)

if (!cmd$dryrun)
{
  grab_repos(repos, localpath=cmd$path, verbose=cmd$verbose)
} else {
  cat("Rerunning without --dryrun will grab:\n\t")
  cat(repos, sep="\n\t")
}
