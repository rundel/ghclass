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



#' Get repository commit count
#'
#' `repo_n_commits` returns a tibble of repositories and the number of commits in a given branch.
#'
#' @param repo   Character. Address of repository in `owner/name` format.
#' @param branch Character.	Branch to list commits from.
#' @param quiet  Logical. Should an error message be printed if a repo does not exist.
#'
#' @examples
#' \dontrun{
#' repo_n_commits("rundel/ghclass")
#' }
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
        # FIXME - work on a staus_msg_v4 function
        # FIXME - retieve error message from graphql response

        usethis::ui_oops(
          glue::glue("Failed to retrieve commits for {usethis::ui_value(repo)}.")
        )

        tibble::tibble(repo = repo,  n = NA_integer_)
      } else {
        tibble::tibble(
          repo = purrr::pluck(res, "data", "repository", "nameWithOwner"),
          n    = purrr::pluck(res, "data", "repository", "object","history", "totalCount", .default=0)
        )
      }
    }
  )
}
