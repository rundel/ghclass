#' @rdname github_token
#' @export
#'
github_rate_limit = function() {
  gh::gh_rate_limit(.token = github_get_token())
}
