github_api_repo_n_commits = function(repo) {
  org = get_repo_owner(repo)
  repo = get_repo_name(repo)

  query = '
    query {
      repository(owner:"{{org}}", name:"{{repo}}") {
        nameWithOwner
        defaultBranchRef {
          name
          target {
            ... on Commit {
              history {
                totalCount
              }
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
#' @param quiet  Logical. Should an error message be printed if a repo does not exist.
#'
#' @export
#'
repo_n_commits = function(repo, quiet = FALSE) {
  arg_is_chr(repo)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = github_api_repo_n_commits(repo)

      if (!is.null(res[["errors"]])) {
        # TODO - work on a status_msg_v4 function
        # TODO - retrieve error message from graphql response

        if (!quiet)
          cli::cli_alert_danger("Failed to retrieve commits for {.val {repo}}.")

        tibble::tibble(
          repo = repo,
          branch = NA_character_,
          n = NA_integer_
        )
      } else {
        tibble::tibble(
          repo = purrr::pluck(res, "data", "repository", "nameWithOwner"),
          branch = purrr::pluck(res, "data", "repository", "defaultBranchRef", "name", .default=NA_character_),
          n = purrr::pluck(res, "data", "repository", "defaultBranchRef", "target", "history", "totalCount", .default=0)
        )
      }
    }
  )
}
