#' @name local_repo
#' @rdname local_repo
#'
#' @title Local repository tools
#'
#' @description
#' The functions provide tools for working with local git repositories, ghclass includes support for following git commands:
#' * git clone = `local_repo_clone`
#' * git add = `local_repo_add`
#' * git commit = `local_repo_commit`
#' * git push = `local_repo_push`
#' * git pull = `local_repo_pull`
#'
#' @param repos GitHub repo names with the form `owner/name`.
#' @param repo_dir Vector of repo directories or a single directory containing one or more repos.
#' @param message commit message
#' @param git Path to the local git binary. [require_git()] attempts to
#' find the git binary based on your `PATH``, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. `--all`).
#' @param verbose Display verbose output.
#'
#' @examples
#' \dontrun{
#' g = org_repos("Sta323-Sp18","hw3-")
#' local_repo_clone(g, "hw3")
#'
#' local_repo_pull(g, "hw3")
#'
#' # After Modifying hw3.Rmd
#' local_repo_add("hw3", "hw3.Rmd")
#' local_repo_commit("hw3", "Revised homework")
#' local_repo_push("hw3")
#' }
#'
NULL


