#' @export
create_branch = function(repo, cur_branch = "master", new_branch, verbose=TRUE) {
  purrr::pwalk(
    list(repo, cur_branch, new_branch),
    function(repo, cur_branch, new_branch) {

      head = get_ref(repo, cur_branch)

      if (verbose)
        message(sprintf("Creating branch %s@(%s => %s) ...", repo, cur_branch, new_branch))


      res = safe_gh("POST /repos/:owner/:repo/git/refs",
                    owner = get_repo_owner(repo),
                    repo = get_repo_name(repo),
                    ref = paste0("refs/heads/",new_branch),
                    sha = head[["sha"]],
                    .token=get_github_token())

      check_result(
        res,
        "Failed to create branch.",
        verbose
      )
    }
  )
}

#' @export
protect_branch = function(repo, branch = "master", verbose = TRUE) {

  stopifnot(!missing(repo))

  purrr::walk2(
    repo, branch,
    function(repo, branch) {

      if (verbose)
        message(sprintf("Protecting branch %s@% ...", repo, branch))

      res = safe_gh(
        "PUT /repos/:owner/:repo/branches/:branch/protection",
        owner = get_repo_owner(repo),
        repo = get_repo_name(repo),
        branch = branch,
        required_status_checks = NA,
        enforce_admins = NA,
        required_pull_request_reviews = NA,
        restrictions = list(
          users = list(),
          teams = list()
        ),
        .token = get_github_token()
      )

      check_result(
        res,
        "Failed to protect branch.",
        verbose
      )
    }
  )
}

#' @export
unprotect_branch = function(repo, branch = "master", verbose = TRUE) {

  stopifnot(!missing(repo))

  purrr::walk2(
    repo, branch,
    function(repo, branch) {

      if (verbose)
        message(sprintf("Unprotecting branch %s@% ...", repo, branch))


      res = safe_gh(
        "DELETE /repos/:owner/:repo/branches/:branch/protection",
        owner = get_repo_owner(repo),
        repo = get_repo_name(repo),
        branch = branch,
        .token = get_github_token()
      )

      check_result(
        res,
        "Failed to unprotect branch.",
        verbose
      )
    }
  )
}
