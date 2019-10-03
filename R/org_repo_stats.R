github_api_org_repo_stats = function(org, inc_commits, inc_issues, inc_prs) {
  org = graphql_quote(org)

  query = '
    query {
      organization(login: {{{org}}}) {
        login
        repositories(first: 100, after: <graphql_quote(cursor)>) {
          totalCount
          pageInfo {
            hasNextPage
            endCursor
          }
          edges {
            node {
              name
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
              closed_prs: pullRequests(states: CLOSED) {
                totalCount
              }
              open_prs: pullRequests(states: OPEN) {
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

  github_api_v4_graphql_paginated(query, page_info = c("organization", "repositories"))
}



#' Get organization repository
#'
#' `org_repo_stats` returns a tibble of repositories belonging to a GitHub organization along with some
#' basic statistics about those repositories.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param inc_commits Logical. Include commit statistics
#' @param inc_issues Logical. Include issue statistics
#' @param inc_prs Logical. Include pull request statistics
#'
#' @examples
#' \dontrun{
#' org_repo_stats("ghclass-demo")
#' }
#'
#' @export
#'
org_repo_stats = function(org, filter = NULL, exclude = FALSE, inc_commits = TRUE, inc_issues = TRUE, inc_prs = TRUE) {
  arg_is_chr_scalar(org)
  arg_is_lgl_scalar(inc_commits, inc_issues, inc_prs)

  pages = github_api_org_repo_stats(org, inc_commits, inc_issues, inc_prs)

  res = purrr::map_dfr(
    pages,
    function(page) {
      org = purrr::pluck(page, "data", "organization", "login")
      repos = purrr::pluck(page, "data", "organization", "repositories", "edges")

      df = tibble::tibble(
        repo          = paste0(org, "/", purrr::map_chr(repos, c("node", "name"))),
        private       = purrr::map_lgl(repos, c("node", "isPrivate"))
      )

      if (inc_commits) {
        df$commits     = purrr::map_int(repos, c("node", "object", "history", "totalCount"), .default=NA)
        df$last_push   = lubridate::ymd_hms(purrr::map_chr(repos, c("node", "pushedAt")))
        df$last_update = lubridate::ymd_hms(purrr::map_chr(repos, c("node", "updatedAt")))
      }

      if (inc_issues) {
        df$open_issues   = purrr::map_int(repos, c("node", "open_issues", "totalCount"))
        df$closed_issues = purrr::map_int(repos, c("node", "closed_issues", "totalCount"))
      }

      if (inc_prs) {
        df$open_prs      = purrr::map_int(repos, c("node", "open_prs", "totalCount"))
        df$closed_prs    = purrr::map_int(repos, c("node", "closed_prs", "totalCount"))
      }

      df
    }
  )

  filter_results(res, "repo", filter, exclude)
}
