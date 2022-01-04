github_api_whoami = function() {
  ghclass_api_v3_req(
    endpoint = "GET /user"
  )
}

#' Returns the login of the authenticated user (based on the current PAT).
#'
#' @param quiet Logical. Should status messages be shown.
#'
#' @return Character value containing user login.
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



github_api_orgs = function() {
  ghclass_api_v3_req(
    endpoint = "GET /user/memberships/orgs"
  )
}

#' Collect details on the authenticated user's GitHub organization memberships
#' (based on the current PAT).
#'
#' @param quiet Logical. Should status messages be shown.
#'
#' @return Returns a tibble with organization details.
#'
#'
#' @examples
#' \dontrun{
#' github_orgs()
#' }
#'
#' @export
#'
github_orgs = function(quiet = FALSE) {
  res = purrr::safely(github_api_orgs)()

  if (!quiet) {
    status_msg(
      res,
      fail = "Failed to retrieve authenticated user's organization memberships."
    )
  }

  res = result(res)
  tibble::tibble(
    org = purrr::map_chr(res, c("organization", "login")),
    role = purrr::map_chr(res, "role"),
    state = purrr::map_chr(res, "state")
  )
}
