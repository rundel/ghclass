#!/usr/bin/env Rscript

suppressMessages(library(ghclass))
suppressMessages(library(optparse))

options = list(
  make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
              help="Print extra output [default]")
)


parser = OptionParser(usage = "%prog [options] target_repo organization pattern", option_list=options)

cmd = parse_args(parser, positional_arguments = TRUE)

if (length(cmd$args) != 3)
{
  message("\nError - Missing argument, 3 are required but ", length(cmd$args)," given.\n")
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

