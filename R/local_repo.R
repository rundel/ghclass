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
#' * git branch = `local_repo_branch`
#' * git log = `local_repo_log`
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
#' local_repo_add(local_repo, "hello.txt")
#' local_repo_commit(local_repo, "Added hello world")
#' local_repo_push(local_repo)
#'
#' repo_commits(repo)
#'
#' # Pulling remote changes
#' repo_modify_file(repo, "hello.txt", pattern = ".*",  content = "!!!", method = "after")
#' local_repo_pull(local_repo)
#' local_repo_log(dir)
#'
#' repo_delete("ghclass-test/local_repo_test", prompt=FALSE)
#' }
#'
NULL


