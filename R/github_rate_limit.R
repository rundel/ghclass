#' @rdname github_token
#' @export
#'
github_rate_limit = function() {
  gh::gh_rate_limit(.token = github_get_token())
}


#' @rdname github_token
#' @export
#'
github_graphql_rate_limit = function() {
  query = '
    query {
      viewer {
        login
      }
      rateLimit {
        limit
        cost
        remaining
        resetAt
      }
    }
  '

  github_api_v4_graphql(query)[[c("data","rateLimit")]]
}
