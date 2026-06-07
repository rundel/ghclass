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
#' * `github_get_max_wait()` / `github_set_max_wait()` - get or set the maximum number
#' of seconds gh will wait when it hits a rate limit. `NULL` (the default) uses gh's
#' own default.
#'
#' * `github_get_max_rate()` / `github_set_max_rate()` - get or set the maximum request
#' rate, in requests per second, used to proactively throttle requests and avoid
#' secondary rate limits. `NULL` (the default) disables throttling.
#'
#' @param limit The maximum number of records to return from an API request.
#' @param max_wait The maximum number of seconds to wait when rate limited, or `NULL`
#' to use gh's default. Passed to [gh::gh()]'s `.max_wait`.
#' @param max_rate The maximum request rate in requests per second, or `NULL` for no
#' throttling. Passed to [gh::gh()]'s `.max_rate`.
#'
#' @details These values are stored in the `"ghclass.api.limit"`, `"ghclass.max.wait"`,
#' and `"ghclass.max.rate"` options globally.
#'
#' @return `github_get_api_limit()` returns a single integer value.
#'
#' `github_get_max_wait()` and `github_get_max_rate()` return their stored value or `NULL`.
#'
#' `github_set_*()` functions invisibly return the value of their argument.
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

#' @rdname github_api_limit
#' @export
#'
github_get_max_wait = function() {
  getOption("ghclass.max.wait", NULL)
}

#' @rdname github_api_limit
#' @export
#'
github_set_max_wait = function(max_wait = NULL) {
  arg_is_pos_int_scalar(max_wait, allow_null = TRUE)
  options("ghclass.max.wait" = max_wait)

  invisible(max_wait)
}

#' @rdname github_api_limit
#' @export
#'
github_get_max_rate = function() {
  getOption("ghclass.max.rate", NULL)
}

#' @rdname github_api_limit
#' @export
#'
github_set_max_rate = function(max_rate = NULL) {
  if (!is.null(max_rate) &&
      (!is.numeric(max_rate) || length(max_rate) != 1L || is.na(max_rate) || max_rate <= 0)) {
    cli_stop("{.arg max_rate} must be a single positive number (requests per second) or {.val NULL}.")
  }

  options("ghclass.max.rate" = max_rate)

  invisible(max_rate)
}
