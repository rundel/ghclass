#' Local repository tools
#'
#' The functions provide tools for working with local git repositories, ghclass includes support for following git commands:
#' * git clone = `clone_repos`
#' * git add = `add_repos`
#' * git commit = `commit_repos`
#' * git push = `push_repos`
#' * git pull = `pull_repos`
#'
#' @param repos GitHub repo names with the form \emph{owner/name}.
#' @param repo_dirs Vector of repo directories or a single directory containing one or more repos.
#' @param message commit message
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--all}).
#' @param verbose Display verbose output.

#' @name local_repo
#'
#' @examples
#' \dontrun{
#' g = get_repos("Sta323-Sp18","hw3-")
#' clone_repos(g, "hw3")
#'
#' pull_repos(g, "hw3")
#'
#' # After Modifying hw3.Rmd
#' add_repos("hw3", "hw3.Rmd")
#' commit_repos("hw3", "Revised homework")
#' push_repos("hw3")
#' }
#'
#' @aliases clone_repos add_repos commit_repos push_repos pull_repos
#'
NULL



# If we are given a single repo directory check if it is a repo or a directory of repos
repo_dirs_helper = function(repo_dirs)
{
  dirs = if (length(repo_dirs) == 1 & !fs::dir_exists(fs::path(repo_dirs[1],".git"))) {
    fs::dir_ls(repo_dirs, type="directory")
  } else {
    repo_dirs
  }

  fs::path_real(dirs)
}


#' @export
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

#' @export
add_repos = function(repo_dirs, files = ".",
                     git = require_git(), options = "", verbose = TRUE)
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




#' @export
commit_repos = function(repo_dirs, message,
                        git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dirs)))
  stopifnot(fs::file_exists(git))
  stopifnot(!missing(message))

  repo_dirs = repo_dirs_helper(repo_dirs)

  purrr::walk2(
    repo_dirs, message,
    function(repo, message) {
      cur_dir = getwd()
      on.exit({
        setwd(cur_dir)
      })
      setwd(repo)

      message = paste0("-m \"", message, "\"")
      cmd = paste(git, "commit", message, options)
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


#' @export
push_repos = function(repo_dirs, remote = "origin", branch="master",
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



#' @export
pull_repos = function(repo_dirs, remote="origin", branch="master",
                      git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dirs)))
  stopifnot(fs::file_exists(git))

  repo_dirs = repo_dirs_helper(repo_dirs)

  purrr::pwalk(
    list(repo_dirs, remote, branch),
    function(repo, remote, branch) {
      cur_dir = getwd()
      on.exit({
        setwd(cur_dir)
      })
      setwd(repo)

      cmd = paste(git, "pull", remote, branch, options)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Pulling ", repo, " (", remote, "/", branch, ") failed.",
                call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)
    }
  )
}


