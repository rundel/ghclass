#!/usr/bin/env Rscript

suppressMessages(library(ghclass))
suppressMessages(library(optparse))

options = list(
  make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
              help="Print extra output [default]"),
  make_option(c("-s", "--suffix"), type="character", default="",
              help="Suffix to use for all repos"),
  make_option(c("-p", "--prefix"), type="character", default="",
              help="Prefix to use for all repos")
)




parser = OptionParser(usage = "%prog [options] organization [team1 team2 ...]", option_list=options)

cmd = parse_args(parser, positional_arguments = TRUE)

str(cmd)

if (length(cmd$args) < 1)
{
  message("\nError - Missing argument, at least 1 are required but ", length(cmd$args)," given.\n")
  print_help(parser)
  quit("no", status=1)
}

teams = NULL
if (length(cmd$args) != 1)
  teams = cmd$args[-1]

create_repos(org = cmd$args[1],
             teams   = teams,
             prefix  = cmd$options$prefix,
             suffix  = cmd$options$suffix,
             verbose = cmd$options$verbose)

