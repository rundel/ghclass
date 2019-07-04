#' @name local_repo
#' @rdname local_repo
#'
#' @title Local repository tools
#'
#' @description
#' The functions provide tools for working with local git repositories, ghclass includes support for following git commands:
#' * git add = `local_repo_add`
#' * git commit = `local_repo_commit`
#' * git push = `local_repo_push`
#' * git pull = `local_repo_pull`
#'
#' @param repo_dir Vector of repo directories or a single directory containing one or more repos.
#' @param files Files to be staged
#' @param message Commit message
#' @param remote Repository remote to use.
#' @param branch Repository branch to use.
#' @param git Path to the local git binary. `require_git()` attempts to
#' find the git binary based on your `PATH``, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. `--all`).
#' @param verbose Display verbose output.
#'
#' @aliases add_repo clone_repo commit_repo pull_repo push_repo
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


