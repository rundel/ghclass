github_api_org_repo_stats = function(org, filter, filter_type, inc_commits, inc_issues, inc_prs) {
  filter_type = paste(filter_type, collapse = ",")

  query = '
    query {
      search(query: "org:{{org}} {{filter}} {{filter_type}}", type: REPOSITORY, first: 100, after: <graphql_quote(cursor)>) {
        repositoryCount
        pageInfo {
          hasNextPage
          endCursor
        }
        edges {
          node {
            ... on Repository {
              nameWithOwner
              isPrivate
              {{#inc_commits}}
              object(expression: "master") {
                ... on Commit {
                  history {
                    totalCount
                  }
                }
              }
              pushedAt
              updatedAt
              {{/inc_commits}}
              {{#inc_issues}}
              closed_issues: issues(states: CLOSED) {
                totalCount
              }
              open_issues: issues(states: OPEN) {
                totalCount
              }
              {{/inc_issues}}
              {{#inc_prs}}
              open_prs: pullRequests(states: OPEN) {
                totalCount
              }
              merged_prs: pullRequests(states: MERGED) {
                totalCount
              }
              closed_prs: pullRequests(states: CLOSED) {
                totalCount
              }
              {{/inc_prs}}
            }
          }
        }
      }
    }
  '

  query = whisker::whisker.render(query)

  github_api_v4_graphql_paginated(query, page_info = c("search"))
}



#' @rdname org_details
#'
#' @param filter_type Character. One or more GitHub search `in` qualifiers.
#' See [documentation](https://help.github.com/en/articles/searching-for-repositories)
#' for more details.
#' @param inc_commits Logical. Include commit statistics
#' @param inc_issues Logical. Include issue statistics
#' @param inc_prs Logical. Include pull request statistics
#'
#' @export
#'
org_repo_stats = function(org, filter = "", filter_type="in:name", inc_commits = TRUE, inc_issues = TRUE, inc_prs = TRUE) {
  flag_experimental()

  arg_is_chr_scalar(org, filter)
  arg_is_chr(org, filter_type)
  arg_is_lgl_scalar(inc_commits, inc_issues, inc_prs)

  pages = github_api_org_repo_stats(org, filter, filter_type, inc_commits, inc_issues, inc_prs)

  purrr::map_dfr(
    pages,
    function(page) {
      repos = purrr::pluck(page, "data", "search", "edges")

      df = tibble::tibble(
        repo          = purrr::map_chr(repos, c("node", "nameWithOwner")),
        private       = purrr::map_lgl(repos, c("node", "isPrivate"))
      )

      if (inc_commits) {
        df$commits     = purrr::map_int(repos, c("node", "object", "history", "totalCount"), .default=NA)
        #df$last_push   = lubridate::ymd_hms(purrr::map_chr(repos, c("node", "pushedAt")))
        df$last_update = lubridate::ymd_hms(purrr::map_chr(repos, c("node", "updatedAt")))
      }

      if (inc_issues) {
        df$open_issues   = purrr::map_int(repos, c("node", "open_issues", "totalCount"))
        df$closed_issues = purrr::map_int(repos, c("node", "closed_issues", "totalCount"))
      }

      if (inc_prs) {
        df$open_prs      = purrr::map_int(repos, c("node", "open_prs", "totalCount"))
        df$merged_prs    = purrr::map_int(repos, c("node", "merged_prs", "totalCount"))
        df$closed_prs    = purrr::map_int(repos, c("node", "closed_prs", "totalCount"))
      }

      df
    }
  )
}
