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

censor_token = function(msg, replacement = "", prefix="", suffix="") {
  pattern = paste0(prefix, get_github_token(), suffix)
  sub(pattern, replacement, msg)
}

run_git = function(git = require_git(), cmd, args = character(), verbose=FALSE) {
  stopifnot(!missing(cmd))

  res = processx::run(
    git, args  = c(cmd, args), error_on_status = FALSE, echo = verbose, echo_cmd = verbose
  )

  err_msg = res[["stderr"]]
  err_msg = sub("fatal: ", "", err_msg)
  err_msg = sub("^\\s|\\s$", "", err_msg)
  err_msg = censor_token(err_msg, suffix="@")

  if (res[["status"]] != 0)
    stop(err_msg)
}


#' @export
clone_repo = function(repo, local_path="./", branch = "master",
                      git = require_git(), options = character(),
                      absolute_path = TRUE, verbose = FALSE)
{
  stopifnot(!missing(repo))
  stopifnot(file.exists(git))

  local_path = fs::path_expand(local_path)

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  purrr::walk2(
    repo, branch,
    function(repo, branch) {
      dir = fs::path(local_path, get_repo_name(repo))

      if (!branch %in% c("", "master"))
        options = c("-b", branch, options)

      res = purrr::safely(run_git)(
        git, "clone", c(options, get_repo_url(repo), dir), verbose = verbose
      )


      fmt_repo = format_repo(repo, branch)

      status_msg(
        res,
        glue::glue("Cloned {usethis::ui_value(fmt_repo)} to {usethis::ui_value(dir)}."),
        glue::glue("Failed to clone {usethis::ui_value(fmt_repo)} to {usethis::ui_value(dir)}.")
      )
    }
  )
}

#' @export
add_repo = function(repo_dir, files = ".",
                    git = require_git(), options = character(),
                    verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk(
    repo_dir,
    function(dir) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "add", c(files, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(files)} to {usethis::ui_value(dir)}."),
        glue::glue("Failed to add {usethis::ui_value(files)} to {usethis::ui_value(dir)}.")
      )
    }
  )
}




#' @export
commit_repo = function(repo_dir, message,
                       git = require_git(), options = character(),
                       verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))
  stopifnot(!missing(message))

  repo_dir = repo_dir_helper(repo_dir)

  purrr::walk2(
    repo_dir, message,
    function(dir, message) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "commit", c("-m", message, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Committed {usethis::ui_value(dir)}."),
        glue::glue("Failed to commit {usethis::ui_value(dir)}.")
      )
    }
  )
}


#' @export
push_repo = function(repo_dir, remote = "origin", branch="master",
                     git = require_git(), options = character(),
                     verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  dir = repo_dir_helper(repo_dir)

  purrr::pwalk(
    list(dir, remote, branch),
    function(dir, remote, branch) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "push", c(remote, branch, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Pushed {usethis::ui_value(dir)}."),
        glue::glue("Failed to push {usethis::ui_value(dir)}.")
      )
    }
  )
}



#' @export
pull_repo = function(repo_dir, remote="origin", branch="master",
                     git = require_git(), options = character(),
                     verbose = FALSE)
{
  stopifnot(all(fs::dir_exists(repo_dir)))
  stopifnot(fs::file_exists(git))

  dir = repo_dir_helper(repo_dir)

  purrr::pwalk(
    list(dir, remote, branch),
    function(dir, remote, branch) {
      withr::local_dir(dir)

      res = purrr::safely(run_git)(
        git, "pull", c(remote, branch, options), verbose = verbose
      )

      status_msg(
        res,
        glue::glue("Pulled {usethis::ui_value(dir)}."),
        glue::glue("Failed to pull {usethis::ui_value(dir)}.")
      )
    }
  )
}


