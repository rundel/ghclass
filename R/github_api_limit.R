#' @rdname github
#' @export
#'
github_get_api_limit = function() {
  getOption("ghclass.api.limit", 1000L)
}

#' @param limit The maximum number of records to return from an API request.
#'
#' @rdname github
#' @export
#'
github_set_api_limit = function(limit = 1000L) {
  options("ghclass.api.limit" = limit)
}
