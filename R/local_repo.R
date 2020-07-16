#' @name local_repo
#' @rdname local_repo
#'
#' @title Functions for managing local git repositories
#'
#' @description
#'
#' * `local_repo_clone` - Clones a GitHub repository to a local directory.
#'
#' * `local_repo_add` - Equivalent to `git add` - stages a file in a local repository.
#'
#' * `local_repo_commit` - Equivalent to `git commit` - commits staged files in a local repository.
#'
#' * `local_repo_push` - Equivalent to `git push` - push a local repository.
#'
#' * `local_repo_pull` - Equivalent to `git pull` - pull a local repository.
#'
#' * `local_repo_branch` - Equivalent to `git branch` - create a branch in a local repository.
#'
#' * `local_repo_log` - Equivalent to `git log` - returns a data frame for git log entries.
#'
#' @param repo_dir Vector of repo directories or a single directory containing one or more repos.
#' @param files Files to be staged
#' @param message Commit message
#' @param remote Repository remote to use.
#' @param branch Repository branch to use.
#' @param mirror Equivalent to `--mirror`
#' @param force Force push?
#' @param prompt Prompt before force push?
#' @param verbose Display verbose output.
#'
#' @details All of these functions depend on the gert library being installed.
#'
#' @examples
#' \dontrun{
#' repo = repo_create("ghclass-test", "local_repo_test")
#'
#' dir = file.path(tempdir(), "repos")
#' local_repo = local_repo_clone(repo, dir)
#'
#' local_repo_log(dir)
#'
#' # Make a local change and push
#' writeLines("Hello World", file.path(local_repo, "hello.txt"))
#'
#' local_repo_add(local_repo, "hello.txt")
#'
#' local_repo_commit(local_repo, "Added hello world")
#'
#' local_repo_push(local_repo)
#'
#' repo_commits(repo)
#'
#' # Pulling remote changes
#' repo_modify_file(repo, "hello.txt", pattern = ".*",  content = "!!!", method = "after")
#'
#' local_repo_pull(local_repo)
#'
#' local_repo_log(dir)
#'
#' repo_delete("ghclass-test/local_repo_test", prompt=FALSE)
#' }
#'
NULL


