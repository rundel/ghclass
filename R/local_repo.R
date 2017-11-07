#' Clone repos from github
#'
#' \code{clone_repos} uses the git binary to clone the provided repositories
#' into a local directory.
#'
#' @param repos Character vector of repo names with the form \emph{owner/name}.
#' @param local_path Local directory into which the repos will be cloned.
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options git binary options (e.g. \code{--bare})
#'
#' @examples
#' \dontrun{
#' clone_repos("rundel/ghclass")
#' }
#'
#' @aliases grab_repos
#'
#' @export
#'
clone_repos = function(repos, local_path="./", git = require_git(), options="")
{
  stopifnot(!missing(repos))
  stopifnot(file.exists(git))

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  walk(repos, function(repo) {
    dir = file.path(local_path, get_repo_name(repo))
    cmd = paste(git, "clone", options, repo_url(repo), dir)
    status = system(
      cmd, intern = FALSE, wait = TRUE,
      ignore.stdout = TRUE, ignore.stderr = TRUE
    )
    if (status != 0)
      message("Cloning ", repo, " failed.")
  })
}

#' @export
grab_repos = function(repos, local_path="./", verbose=TRUE)
{
  .Deprecated("clone_repos")
  clone_repos(repos,local_path)
}

#' Update local repos via git pull
#'
#' \code{clone_repos} uses the git binary to clone the provided repositories
#' into a local directory.
#'
#' @param repos Character vector of repo names with the form \emph{owner/name}.
#' @param local_path Local directory into which the repos will be cloned.
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options git binary options (e.g. \code{--bare})
#'
#' @examples
#' \dontrun{
#' clone_repos("rundel/ghclass")
#' }
#'
#' @aliases grab_repos
#'
#' @export
#'

update_repos = function(repo_dirs, git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(dir.exists(repo_dirs))

  # If we are given a single repo directory check if it is a repo or a directory of repos
  if (   length(repo_dirs)==1
      & !any(dir.exists(file.path(repo_dirs,".git"))))
    repo_dirs = list.dirs(repo_dirs, full.names=TRUE, recursive = FALSE)

  walk(repo_dirs, update_repo, git=git, options=options, verbose=verbose)
}


#' @export
update_repo = function(repo, git, options, verbose)
{
  cur_dir = getwd()
  on.exit({setwd(cur_dir)})

  setwd(normalizePath(repo))

  cmd = paste(git, "pull", options)
  status = system(
    cmd, intern = FALSE, wait = TRUE,
    ignore.stdout = !verbose, ignore.stderr = !verbose
  )
  if (status != 0)
    message("Updating ", repo, " (via pull) failed.")
}

#' @export
compile_rmds = function(local_path)
{
  # local_path = args[1]
  #
  # stopifnot(dir.exists(local_path))
  #
  # repos = list.dirs(path = local_path, full.names = TRUE, recursive = FALSE)
  #
  # prev_dir = getwd()
  #
  # for(repo in repos)
  # {
  #   cat("Knitting in", repo, "...\n")
  #
  #   setwd(repo)
  #
  #   try({
  #     rmds = list.files("./", pattern="[Rr]md$")
  #     lapply(rmds, render, quiet=TRUE)
  #   })
  #
  #   setwd(prev_dir)
  # }
}


