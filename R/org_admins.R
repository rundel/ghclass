github_api_org_admins = function(owner){
  gh::gh("GET /orgs/:owner/members",
         owner = owner,
         role = "admin",
         .token = github_get_token(),
         .limit = get_github_api_limit())
}

#' List repository administrators
#'
#' @param org Character. Name of a GitHub organization.
#'
#' @examples
#' \dontrun{
#' org_admins("Sta523-Fa17")
#' }
#'
#' @return A character vector of repository administrators.
#'
#' @aliases get_admin
#'
#' @export
#'
org_admins = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org_admins)(owner = org)

  if (failed(res))
    usethis::ui_stop(glue::glue("Failed to retrieve admins for org {usethis::ui_value(org)}."))
  else
    purrr::map_chr(result(res), "login")
}
