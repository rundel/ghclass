github_api_repo_n_commits = function(repo, branch) {
  org = get_repo_owner(repo)
  name = get_repo_name(repo)

  query = '
    query {
      repository(owner:"{{org}}", name:"{{name}}") {
        nameWithOwner
        object(expression:"{{branch}}") {
          ... on Commit {
            history {
              totalCount
            }
          }
        }
      }
    }
  '

  query = whisker::whisker.render(query)

  github_api_v4_graphql(query)
}



#' @rdname repo_details
#'
#' @param repo   Character. Address of repository in `owner/name` format.
#' @param branch Character.	Branch to list commits from.
#' @param quiet  Logical. Should an error message be printed if a repo does not exist.
#'
#' @export
#'
repo_n_commits = function(repo, branch = "master", quiet = FALSE) {
  arg_is_chr(repo, branch)

  purrr::map2_dfr(
    repo, branch,
    function(repo, branch) {
      res = github_api_repo_n_commits(repo, branch)

      if (!is.null(res[["errors"]]) & !quiet) {
        # TODO - work on a status_msg_v4 function
        # TODO - retrieve error message from graphql response

        cli::cli_alert_danger("Failed to retrieve commits for {.val {repo}}.")

        tibble::tibble(
          repo = repo,
          branch = branch,
          n = NA_integer_
        )
      } else {
        tibble::tibble(
          repo = purrr::pluck(res, "data", "repository", "nameWithOwner"),
          branch = branch,
          n    = purrr::pluck(res, "data", "repository", "object","history", "totalCount", .default=0)
        )
      }
    }
  )
}
