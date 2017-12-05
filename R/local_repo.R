#' Clone repos from github
#'
#' \code{clone_repos} uses the git binary to clone the provided repositories
#' into a local directory.
#'
#' @param repos Character vector of repo names with the form \emph{owner/name}.
#' @param local_path Local directory into which the repos will be cloned.
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--bare})
#' @param verbose Display verbose output.
#'
#' @examples
#' \dontrun{
#' clone_repos("rundel/ghclass")
#' }
#'
#' @aliases grab_repos
#'
#' @family local repo functions
#'
#' @export
#'
clone_repos = function(repos, local_path="./", git = require_git(), options="", verbose=FALSE)
{
  stopifnot(!missing(repos))
  stopifnot(file.exists(git))

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  walk(repos, function(repo) {
    dir = file.path(local_path, get_repo_name(repo))
    cmd = paste(git, "clone", options, repo_url(repo), dir)
    status = system(
      cmd, intern = FALSE, wait = TRUE,
      ignore.stdout = !verbose, ignore.stderr = !verbose
    )
    if (status != 0)
      warning("Cloning ", repo, " failed.", call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
  })
}

#' @export
#'
grab_repos = function(repos, local_path="./", verbose=TRUE)
{
  .Deprecated("clone_repos")
  clone_repos(repos,local_path)
}



#' Update local repos via git pull
#'
#' \code{update_repos} uses the git binary to update the provided repositories
#' using git pull.
#'
#' @param repos_dir Character vector of repo directories or a single directory containing the repo directories.
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--all}).
#' @param verbose Display verbose output.
#'
#' @examples
#' \dontrun{
#' update_repos("hw1/")
#' update_repos(c("hw1/team01","hw1/team02","hw1/team03"))
#' }
#'
#' @family local repo functions
#'
#' @export
#'
update_repos = function(repo_dirs, git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(dir.exists(repo_dirs))
  stopifnot(file.exists(git))

  # If we are given a single repo directory check if it is a repo or a directory of repos
  if (length(repo_dirs) == 1 & !any(dir.exists(file.path(repo_dirs,".git"))))
    repo_dirs = list.dirs(repo_dirs, full.names=TRUE, recursive = FALSE)

  walk(
    repo_dirs,
    function(repo) {
      cur_dir = getwd()
      on.exit({setwd(cur_dir)})

      setwd(normalizePath(repo))

      cmd = paste(git, "pull", options)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Updating ", repo, " (via pull) failed.", call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    }
  )
}


require_git = function()
{
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found via the PATH variable.")
  }

  return(git)
}

