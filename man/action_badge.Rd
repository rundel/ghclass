% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/action.R, R/action_badge.R
\name{action_badge}
\alias{action_badge}
\alias{action_add_badge}
\alias{action_remove_badge}
\title{Add or remove GitHub Actions badges from a repository}
\usage{
action_add_badge(
  repo,
  workflow = NULL,
  where = "^.",
  line_padding = "\\n\\n\\n",
  file = "README.md"
)

action_remove_badge(repo, workflow_pat = ".*?", file = "README.md")
}
\arguments{
\item{repo}{Character. Address of repository in \code{owner/name} format.}

\item{workflow}{Character. Name of the workflow.}

\item{where}{Character. Regex pattern indicating where to insert the badge, defaults
to the beginning of the target file.}

\item{line_padding}{Character. What text should be added after the badge.}

\item{file}{Character. Target file to be modified, defaults to \code{README.md}.#'}

\item{workflow_pat}{Character. Name of the workflow to be removed, or a regex pattern
that matches the workflow name.}
}
\value{
Both \code{action_add_badge()} and \code{action_remove_badge()} invisibly return a list
containing the results of the relevant GitHub API call.
}
\description{
\itemize{
\item \code{action_add_badge()} - Add a GitHub Actions badge to a file.
\item \code{action_remove_badge()} - Remove one or more GitHub Action badges from a file.
}
}
