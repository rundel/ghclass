github_api_get_invitations = function(owner){
  arg_is_chr_scalar(owner)
  gh::gh("GET /orgs/:owner/invitations",
         owner = owner,
         .token = github_get_token(),
         .limit = get_github_api_limit())
}


#' Get pending organization members
#'
#' `get_pending_member` returns a (filtered) vector of pending organization memebers.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repos.
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_pending_member("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_pending_member = function(org, filter = NULL, exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_get_invitations)(org)
  status_msg(
    res,
    fail = glue::glue("Failed to retrieve pending members for org {usethis::ui_value(org)}")
  )

  invite = purrr::map(result(res), "login")
  invite = purrr::flatten_chr(invite)
  filter_results(invite, filter, exclude)
}
