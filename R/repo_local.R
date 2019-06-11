#' Local repository tools
#'
#' The functions provide tools for working with local git repositories, ghclass includes support for following git commands:
#' * git clone = `clone_repo`
#' * git add = `add_repo`
#' * git commit = `commit_repo`
#' * git push = `push_repo`
#' * git pull = `pull_repo`
#'
#' @param repos GitHub repo names with the form \emph{owner/name}.
#' @param repo_dir Vector of repo directories or a single directory containing one or more repos.
#' @param message commit message
#' @param git Path to the local git binary. \code{require_git()} attempts to
#' find the git binary based on your \code{PATH}, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. \code{--all}).
#' @param verbose Display verbose output.

#' @name local_repo
#'
#' @examples
#' \dontrun{
#' g = get_repo("Sta323-Sp18","hw3-")
#' clone_repo(g, "hw3")
#'
#' pull_repo(g, "hw3")
#'
#' # After Modifying hw3.Rmd
#' add_repo("hw3", "hw3.Rmd")
#' commit_repo("hw3", "Revised homework")
#' push_repo("hw3")
#' }
#'
#' @aliases clone_repo add_repo commit_repo push_repo pull_repo
#'
NULL


# If we are given a single repo directory check if it is a repo or a directory of repos
repo_dir_helper = function(repo_dir) {
  if (length(repo_dir) == 1 & !fs::dir_exists(fs::path(repo_dir[1],".git"))) {
    dir = fs::dir_ls(repo_dir, type="directory")
  } else {
    dir = repo_dir
  }

  fs::path_real(dir)
}


#' @export
rename_local_repo = function(repo_dir, pattern, replacement) {
  stopifnot(length(repo_dir) == 1)
  stopifnot(length(pattern) == length(replacement))

  repos = repo_dir_helper(repo_dir)
  cur_repos = repos

  for(i in seq_along(pattern)) {
    repos = sub(pattern[i],replacement[i], repos)
  }

  sub = repos != cur_repos
  purrr::walk2(
    cur_repos[sub], repos[sub],
    function(cur, new) {
      res = purrr::safely(fs::file_move)(cur, new)
      status_msg(
        res,
        glue::glue("Renaming {usethis::ui_value(cur)} to {usethis::ui_value(to)}."),
        glue::glue("Failed to rename {usethis::ui_value(cur)} to {usethis::ui_value(to)}.")
      )
    }
  )
}


run_git = function(git = require_git(), cmd, args = character(), verbose=FALSE) {
  stopifnot(!missing(cmd))

  processx::run(
    git, args  = c(cmd, args), error_on_status = TRUE, echo = verbose
  )

  invisible(NULL)
}


#' @export
clone_repo = function(repo, local_path="./", branch = "master",
                      git = require_git(), options = character(),
                      absolute_path = TRUE, verbose = TRUE)
{
  stopifnot(!missing(repo))
  stopifnot(file.exists(git))

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  res = purrr::map2_chr(
    repo, branch,
    function(repo, branch) {
      dir = fs::path(local_path, get_repo_name(repo))

      if (branch != "")
        branch = paste("-b", branch)

      if (verbose)
        message("Cloning ", repo, " to ", dir, " ...")

      cmd = paste(git, "clone", branch, options, get_repo_url(repo), dir)
      status = system(
        cmd, intern = FALSE, wait = TRUE,
        ignore.stdout = !verbose, ignore.stderr = !verbose
      )
      if (status != 0)
        warning("Cloning failed.", call. = FALSE, immediate. = TRUE, noBreaks. = TRUE)

      if (absolute_path)
        dir = fs::path_real(dir)

      dir
    }
  )

  invisible(res)
}

#' @export
add_repo = function(repo_dir, files = ".",
                    git = require_git(), options = "", verbose = TRUE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk(
    repo_dir,
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
commit_repo = function(repo_dir, message,
                       git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))
  stopifnot(!missing(message))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk2(
    repo_dir, message,
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
push_repo = function(repo_dir, remote = "origin", branch="master",
                     git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk2(
    repo_dir,
    remote,
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
pull_repo = function(repo_dir, remote="origin", branch="master",
                     git = require_git(), options = "", verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::pwalk(
    list(repo_dir, remote, branch),
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


