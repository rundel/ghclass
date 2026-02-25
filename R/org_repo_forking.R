github_api_org_repo_forking = function(org) {
  query = '
    query {
      organization(login: "{{org}}") {
        repositories(first: 100, after: <graphql_quote(cursor)>, privacy: PRIVATE) {
          totalCount
          pageInfo {
            hasNextPage
            endCursor
          }
          nodes {
            nameWithOwner
            forkingAllowed
            forkCount
          }
        }
      }
    }
  '

  query = whisker::whisker.render(query)

  github_api_v4_graphql_paginated(query, page_info = c("organization", "repositories"))
}


#' @rdname org_details
#' @export
#'
org_repo_forking = function(org) {
  arg_is_chr_scalar(org)

  pages = github_api_org_repo_forking(org)

  purrr::map_dfr(
    pages,
    function(page) {
      nodes = purrr::pluck(page, "data", "organization", "repositories", "nodes")

      tibble::tibble(
        repo          = purrr::map_chr(nodes, "nameWithOwner"),
        allow_forking = purrr::map_lgl(nodes, "forkingAllowed"),
        n_forks       = purrr::map_int(nodes, "forkCount")
      )
    }
  )
}
