github_api_org_pending = function(owner){
  arg_is_chr_scalar(owner)
  gh::gh("GET /orgs/:owner/invitations",
         owner = owner,
         .token = github_get_token(),
         .limit = github_get_api_limit())
}


#' Get pending organization members
#'
#' `org_pending` returns a (filtered) vector of pending organization memebers.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repos.
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#'
#' @examples
#' \dontrun{
#' org_pending("ghclass")
#' }
#'
#' @export
#'
org_pending = function(org, filter = NULL, exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_org_pending)(org)
  status_msg(
    res,
    fail = glue::glue("Failed to retrieve pending members for org {usethis::ui_value(org)}")
  )

  invite = purrr::map(result(res), "login")
  invite = purrr::flatten_chr(invite)
  filter_results(invite, filter, exclude)
}

#' @export
#'
org_pending_members = function(org, filter = NULL, exclude = FALSE) {
  .Deprecated("org_pending")
  org_pending(org, filter, exclude)
}
