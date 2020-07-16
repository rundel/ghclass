github_api_whoami = function() {
  gh::gh(
    "GET /user",
    .token = github_get_token()
  )
}

#' Returns the login of the authenticated user (based on the current PAT).
#'
#' @param quiet Logical. Should status messages be shown.
#'
#' @examples
#' \dontrun{
#' github_whoami()
#' }
#'
#' @export
#'
github_whoami = function(quiet = FALSE) {
  res = purrr::safely(github_api_whoami)()

  if (!quiet) {
    status_msg(
      res,
      fail = "Failed to retrieve whoami results."
    )
  }

  result(res)[["login"]]
}
