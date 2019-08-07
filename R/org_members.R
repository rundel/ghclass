github_api_org_members = function(org) {
  arg_is_chr_scalar(org)

  gh::gh("GET /orgs/:org/members",
         org = org,
         .token = github_get_token(),
         .limit = github_get_api_limit())
}


#' Get organization member
#'
#' `org_members` returns a (filtered) vector of organization memebers.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#' @param include_admins Logical. Should admin users be included in the results.
#'
#' @examples
#' \dontrun{
#' org_members("ghclass")
#' }
#'

#' @aliases get_member get_members
#'
#' @export
#'
org_members = function(org, filter = NULL, exclude = FALSE,
                      include_admins = TRUE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_org_members)(org)
  members = purrr::map_chr(result(res), "login")

  if (!include_admins)
    members = setdiff(members, org_admins(org))

  filter_results(members, filter, exclude)
}
