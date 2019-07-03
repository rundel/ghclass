#' @name local_repo
#' @rdname local_repo
#'
#' @title Local repository tools
#'
#' @description
#' The functions provide tools for working with local git repositories, ghclass includes support for following git commands:
#' * git clone = `clone_repo`
#' * git add = `add_repo`
#' * git commit = `commit_repo`
#' * git push = `push_repo`
#' * git pull = `pull_repo`
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
#' @aliases local_repo_rename
#'
NULL


