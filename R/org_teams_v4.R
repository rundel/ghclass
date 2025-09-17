github_api_org_teams_v4 = function(org) {
  query = '
    query {
      organization(login: "{{org}}") {
        teams(first: 100, after: <graphql_quote(cursor)>) {
          pageInfo {
             hasNextPage
             endCursor
          }
          edges {
            node {
              id
              name
              slug
              privacy
            }
          }
        }
      }
    }
  '

  query = whisker::whisker.render(query)

  github_api_v4_graphql_paginated(query, page_info = c("organization", "teams"))
}


org_team_details_v4 = function(org) {
  #warn_experimental()
  arg_is_chr_scalar(org)

  pages = github_api_org_teams_v4(org)

  purrr::map_dfr(
    pages,
    function(page) {
      teams = purrr::pluck(page, "data", "organization", "teams", "edges")

      tibble::tibble(
        org  = org,
        id   = purrr::map_chr(teams, c("node", "id")),
        name = purrr::map_chr(teams, c("node", "name")),
        slug   = purrr::map_chr(teams, c("node", "slug")),
        privacy = purrr::map_chr(teams, c("node", "privacy")) |> tolower(),
      )
    }
  )
}
