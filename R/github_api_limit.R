#' @name github_api_limit
#' @rdname github_api_limit
#'
#' @title Tools for limiting gh's GitHub api requests.
#'
#' @description
#' * `github_get_api_limit()` - returns the current limit on results returned by gh.
#'
#' * `github_set_api_limit()` - sets a limit on results returned by gh.
#'
#' @param limit The maximum number of records to return from an API request.
#'
#' @details This value is stored in the "ghclass.api.limit" option globally.
#'
#' @return `github_get_api_limit()` returns a single integer value.
#'
#' `github_set_api_limit()` invisibily returns the value of the `limit` argument.
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
  getOption("ghclass.api.limit", 10000L)
}

#' @rdname github_api_limit
#' @export
#'
github_set_api_limit = function(limit = 10000L) {
  arg_is_pos_int(limit)
  options("ghclass.api.limit" = limit)

  invisible(limit)
}
