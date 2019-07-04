github_api_get_admin = function(owner){
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
#' get_admin("Sta523-Fa17")
#' }
#'
#' @return A character vector of repository administrators.
#'
#' @export
#'
get_admin = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_get_admin)(owner = org)

  if (failed(res))
    usethis::ui_stop(glue::glue("Failed to retrieve admuns for org {usethis::ui_value(org)}."))
  else
    purrr::map_chr(result(res), "login")
}
