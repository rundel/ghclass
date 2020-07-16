#' @name github_api_limit
#' @rdname github_api_limit
#'
#' @title Tools for limiting gh's GitHub api requests.
#'
#' @description
#' * `github_get_api_limit` - returns the current limit on results returned by gh.
#'
#' * `github_set_api_limit` - sets a limit on results returned by gh.
#'
#' @param limit The maximum number of records to return from an API request.
#'
#' @details This value is stored in the "ghclass.api.limit" option globally.
#'
#' @examples
#' github_get_api_limit()
#'
#' github_set_api_limit(500)
#'
#' github_get_api_limit()
NULL



#' @rdname github_api_limit
#' @export
#'
github_get_api_limit = function() {
  getOption("ghclass.api.limit", 1000L)
}

#'
#' @rdname github_api_limit
#' @export
#'
github_set_api_limit = function(limit = 1000L) {
  options("ghclass.api.limit" = limit)
}
