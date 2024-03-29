% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo.R, R/repo_unwatch.R, R/repo_watch.R,
%   R/repo_watching.R
\name{repo_notification}
\alias{repo_notification}
\alias{repo_unwatch}
\alias{repo_watch}
\alias{repo_ignore}
\alias{repo_watching}
\title{GitHub Repository tools - notification functions}
\usage{
repo_unwatch(repo)

repo_watch(repo)

repo_ignore(repo)

repo_watching(filter = NULL, exclude = FALSE)
}
\arguments{
\item{repo}{repository address in \code{owner/repo} format}

\item{filter}{character, regex pattern for matching (or excluding) repositories.}

\item{exclude}{logical, should entries matching the regex be excluded or included.}
}
\value{
\code{repo_ignore()}, \code{repo_unwatch()}, and \code{repo_watch()} all invisibly return a
list containing the results of the relevant GitHub API call.

\code{repo_watching()} returns a character vector of watched repos.
}
\description{
\itemize{
\item \code{repo_ignore()} - Ignore a GitHub repository.
\item \code{repo_unwatch()} - Unwatch / unsubscribe from a GitHub repository.
\item \code{repo_watch()} - Watch / subscribe to a GitHub repository.
\item \code{repo_watching()} - Returns a vector of your watched repositories. This should
match the list at \href{https://github.com/watching}{github.com/watching}.
}
}
\examples{
\dontrun{
repo_ignore("Sta323-Sp19/hw1")

repo_unwatch("rundel/ghclass")

repo_watch("rundel/ghclass")
}

}
