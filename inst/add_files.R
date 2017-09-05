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
  make_option(c("-b", "--branch"), type = "character", default="master",
              dest="branch", help="Which branch to use [default \"%default\"]"),
  make_option(c("-m", "--message"), type = "character", default="Adding files",
              help="Commit message [default \"%default\"]"),
  make_option(c("--preserve"), action="store_true", default=FALSE,
              help="Perserve full path when commiting to github"),
  make_option(c("--dryrun"), action="store_true", default=FALSE,
              help="Don't actually add files, describe what would happen when run.")
)


parser = OptionParser(usage = "%prog [options] file1 [file2 ...]", option_list=options)

cmd = parse_args(parser, positional_arguments = TRUE)

if (length(cmd$args) < 1)
{
  message("\nError - Missing file arguments, at least 1 is required.\n")
  print_help(parser)
  quit("no", status=1)
}

repos = c()
if (!is.null(cmd$options$org))
  repos = get_repos(cmd$options$org, cmd$options$filter, cmd$options$exclude, full_repo=TRUE)

if (!is.null(cmd$options$repos))
  repos = c(repos, str_split(cmd$options$repos,",")[[1]])

require_valid_repo(repos, require_owner=TRUE)

if (!cmd$options$dryrun)
{
  add_files(repos, files = cmd$args,
            message = cmd$options$message, branch = cmd$options$branch,
            preserve_path = cmd$options$preserve, verbose=cmd$options$verbose)
} else {
  cat("Rerunning without --dryrun will add:\n\t")
  cat(cmd$args, sep="\n\t")
  cat("\bto the following repositories:\n\t")
  cat(repos, sep="\n\t")
}
