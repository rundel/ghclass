#!/usr/bin/env Rscript

suppressMessages(library(ghclass))
suppressMessages(library(optparse))

options = list(
  make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
              help="Print extra output [default]"),
  make_option(c("-b", "--branch"), type = "character", default="master",
              dest="branch", help="Which branch to use [default \"%default\"]"),
  make_option(c("-m", "--message"), type="character", default="Adding files",
              help="Commit message [default \"%default\"]"),
  make_option(c("-p", "--preserve"), action="store_true", default=FALSE,
              help="Perserve full path when commiting to github")
)


parser = OptionParser(usage = "%prog [options] organization pattern file1 [file2 ...]", option_list=options)

cmd = parse_args(parser, positional_arguments = TRUE)

if (length(cmd$args) < 3)
{
  message("\nError - Missing arguments, at least 3 are required but ", length(cmd$args)," given.\n")
  print_help(parser)
  quit("no", status=1)
}

add_files(org = cmd$args[1], pattern = cmd$args[2], files = cmd$args[-(1:2)],
          message = cmd$options$message, branch = cmd$options$branch,
          preserve_path = cmd$options$preserve, verbose=cmd$options$verbose)
