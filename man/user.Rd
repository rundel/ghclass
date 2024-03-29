% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user.R, R/user_exists.R, R/user_repos.R,
%   R/user_type.R
\name{user}
\alias{user}
\alias{user_exists}
\alias{user_repos}
\alias{user_type}
\title{GitHub user related tools}
\usage{
user_exists(user)

user_repos(
  user,
  type = c("owner", "all", "public", "private", "member"),
  filter = NULL,
  exclude = FALSE,
  full_repo = TRUE
)

user_type(user)
}
\arguments{
\item{user}{Character. GitHub username(s).}

\item{type}{Character. Can be one of "all", "owner", "public", "private", "member".}

\item{filter}{Character. Regular expression pattern for matching (or excluding) repositories.}

\item{exclude}{Logical. Should entries matching the regular expression in \code{filter} be excluded or included?}

\item{full_repo}{Logical. Should the full repository address be returned
(e.g. \code{owner/repo} instead of just \code{repo})?}
}
\value{
\code{user_exists()} returns a logical vector.

\code{user_repos()} and \code{user_type()} retrun a character vector.
}
\description{
\itemize{
\item \code{user_exists()} - returns \code{TRUE} if the username(s) (or organization) exist on GitHub
and \code{FALSE} otherwise. Note that GitHub considers organizations to be a type of user.
\item \code{user_repos()} - returns a (filtered) vector of repositories belonging to the user.
\item \code{user_type()} - returns a vector of the accounts' types.
}
}
\examples{
\dontrun{
user_exists(c("rundel", "ghclass-test", "hopefullydoesnotexist"))

user_repos("rundel", type = "public", filter = "ghclass")

user_repos("ghclass-test")

org_repos("ghclass-test")

user_type(c("rundel", "ghclass-test"))
}
}
