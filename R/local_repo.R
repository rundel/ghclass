#' Clone repos from github
#'
#' \code{clone_repos} uses the git binary to clone the provided repositories
#' into a local directory. The function returns a character vector of paths for the cloned repos.
#'
#' @param repos Vector of repo names with the form \emph{owner/name}.
#' @param local_path Local directory into which the repos will be cloned.
#' @param branch Name of branch(es) to clone, defaults to master.
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--bare})
#' @param absolute_path Should absolute path be returned for cloned repos.
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
clone_repos = function(repos, local_path="./", branch = "master",
                       git = require_git(), options="", absolute_path=TRUE,
                       verbose=FALSE)
{
  stopifnot(!missing(repos))
  stopifnot(file.exists(git))

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  invisible(map2_chr(
    repos, branch,
    function(repo, branch) {
      dir = fs::path(local_path, get_repo_name(repo))

      if (branch != "")
        branch = paste("-b", branch)


      cmd = paste(git, "clone", branch, options, get_repo_url(repo), dir)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Cloning ", repo, " failed.", call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)

      if (absolute_path)
        dir = fs::path_real(dir)

      dir
    }
  ))
}


#' Push local repos via git
#'
#' \code{push_repos} uses the git binary to push local changes to the provided remote.
#'
#' @param repo_dirs Vector of repo directories or a single directory containing one or more repos.
#' @param remotes Vector of remote names or urls
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
#' @aliases pull_repos
#'
#' @family local repo functions
#'
#' @export
#'
push_repos = function(repo_dirs, remotes = "origin master",
                      git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dirs)))
  stopifnot(fs::file_exists(git))

  repo_dirs = repo_dirs_helper(repo_dirs)

  walk2(
    repo_dirs,
    remotes,
    function(repo, remote) {
      cur_dir = getwd()
      on.exit({
        setwd(cur_dir)
      })
      setwd(repo)

      cmd = paste(git, "push", remote, options)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Push changes to ", repo, "  failed.",
                call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    }
  )
}


#' Update local repos via git pull
#'
#' \code{update_repos} uses the git binary to update the provided repositories
#' using git pull.
#'
#' @param repo_dirs Vector of repo directories or a single directory containing one or more repos.
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
#' @aliases pull_repos
#'
#' @family local repo functions
#'
#' @export
#'
update_repos = function(repo_dirs, git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dirs)))
  stopifnot(fs::file_exists(git))

  repo_dirs = repo_dirs_helper(repo_dirs)

  walk(
    repo_dirs,
    function(repo) {
      cur_dir = getwd()
      on.exit({
        setwd(cur_dir)
      })
      setwd(repo)

      cmd = paste(git, "pull", options)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Updating ", repo, " (via pull) failed.",
                call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    }
  )
}

#' @export
#'
pull_repos = update_repos



#' Add changes for local repos via git add
#'
#' \code{add_repos} uses the git binary to update the provided repositories
#' using git pull.
#'
#' @param repo_dirs Vector of repo directories or a single directory containing one or more repos.
#' @param files Vector of files to add
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--all}).
#' @param verbose Display verbose output.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @family local repo functions
#'
#' @export
#'
add_repos = function(repo_dirs, files = ".", git = require_git(), options = "", verbose = TRUE)
{
  stopifnot(all(fs::dir_exists(repo_dirs)))
  stopifnot(fs::file_exists(git))

  repo_dirs = repo_dirs_helper(repo_dirs)

  print(repo_dirs)

  walk(
    repo_dirs,
    function(repo) {
      cur_dir = getwd()
      on.exit({
        setwd(cur_dir)
      })
      setwd(repo)

      files = paste(files, collapse=" ")
      cmd = paste(git, "add", files, options)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Adding files to ", repo, " failed.",
                call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    }
  )
}



#' Commit changes for local repos via git commit
#'
#' \code{commit_repos} uses the git binary to update the provided repositories
#' using git pull.
#'
#' @param repo_dirs Vector of repo directories or a single directory containing one or more repos.
#' @param msg Commit message
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--all}).
#' @param verbose Display verbose output.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @family local repo functions
#'
#' @export
#'
commit_repos = function(repo_dirs, msg, git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dirs)))
  stopifnot(fs::file_exists(git))
  stopifnot(!missing(msg))

  repo_dirs = repo_dirs_helper(repo_dirs)

  walk(
    repo_dirs,
    function(repo) {
      cur_dir = getwd()
      on.exit({
        setwd(cur_dir)
      })
      setwd(repo)

      msg = paste0("-m \"", msg, "\"")
      cmd = paste(git, "commit", msg, options)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Commiting to ", repo, " failed.",
                call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    }
  )
}






repo_dirs_helper = function(repo_dirs) {
  # If we are given a single repo directory check if it is a repo or a directory of repos

  dirs = if (length(repo_dirs) == 1 & !fs::dir_exists(fs::path(repo_dirs[1],".git"))) {
    fs::dir_ls(repo_dirs, type="directory")
  } else {
    repo_dirs
  }

  fs::path_real(dirs)
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

