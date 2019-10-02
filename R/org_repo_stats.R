github_api_org_repo_stats = function(org) {
  query = '
    query {
      organization(login: <graphql_quote(org)>) {
        login
        repositories(first: 100, after: <graphql_quote(cursor)>) {
          totalCount
          pageInfo {
            hasNextPage
            endCursor
          }
          pageInfo {
            endCursor
          }
          edges {
            node {
              name
              isPrivate
              object(expression: "master") {
      					... on Commit {
        					history {
          					totalCount
        					}
      					}
    					}
              closed_issues: issues(states: CLOSED) {
                totalCount
              }
              open_issues: issues(states: OPEN) {
                totalCount
              }
              closed_prs: pullRequests(states: CLOSED) {
                totalCount
              }
              open_prs: pullRequests(states: OPEN) {
                totalCount
              }
              pushedAt
              updatedAt
            }
          }
        }
      }
    }
  '

  github_api_v4_graphql_paginated(query, vars = list(org = org), page_info = c("organization", "repositories"))
}



#' Get organization repository
#'
#' `org_repo_stats` returns a tibble of repositories belonging to a GitHub organization along with some
#' basic statistics about those repositories.
#'
#' @param org Character. Name of the GitHub organization.
#'
#' @examples
#' \dontrun{
#' org_repo_stats("ghclass-demo")
#' }
#'
#' @export
#'
org_repo_stats = function(org) {
  arg_is_chr_scalar(org)

  pages = github_api_org_repo_stats(org)

  purrr::map_dfr(
    pages,
    function(page) {
      org = purrr::pluck(page, "data", "organization", "login")
      repos = purrr::pluck(page, "data", "organization", "repositories", "edges")

      tibble::tibble(
        repo          = paste0(org, "/", purrr::map_chr(repos, c("node", "name"))),
        private       = purrr::map_lgl(repos, c("node", "isPrivate")),
        commits       = purrr::map_int(repos, c("node", "object", "history", "totalCount"), .default=NA),
        open_issues   = purrr::map_int(repos, c("node", "open_issues", "totalCount")),
        closed_issues = purrr::map_int(repos, c("node", "closed_issues", "totalCount")),
        open_prs      = purrr::map_int(repos, c("node", "open_prs", "totalCount")),
        closed_prs    = purrr::map_int(repos, c("node", "closed_prs", "totalCount")),
        last_push     = lubridate::ymd_hms(purrr::map_chr(repos, c("node", "pushedAt"))),
        last_update   = lubridate::ymd_hms(purrr::map_chr(repos, c("node", "updatedAt")))
      )
    }
  )
}
