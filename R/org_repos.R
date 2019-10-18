github_api_org_repos = function(owner) {
  arg_is_chr_scalar(owner)

  gh::gh(
    "GET /orgs/:owner/repos",
    owner = owner,
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' Get organization repository
#'
#' `org_repos` returns a (filtered) vector of repositories belonging to a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param full_repo Logical. Should the full repository address be returned (e.g. `owner/name` instead of just `repo`)?
#'
#'
#' @examples
#' \dontrun{
#' org_repos("ghclass")
#' org_repos("ghclass", "hw1-")
#' }
#'

#' @aliases get_repo get_repos
#'
#' @export
#'
org_repos = function(org, filter = NULL, exclude = FALSE, full_repo = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(
    function() {
      type = user_type(org)

      if (is.na(type)) {
        usethis::ui_stop( paste(
          "Organization {usethis::ui_value(org)} does not exist on GitHub."
        ) )
      } else if (type == "Organization") {
        github_api_org_repos(org)
      } else if (type == "User") {
        usethis::ui_stop( paste(
          "{usethis::ui_value(org)} is a user not an organization.",
          "Use {usethis::ui_code('user_repos')} instead."
        ) )
      } else {
        usethis::ui_stop( paste(
          "{usethis::ui_value(org)} has unknown type {usethis::ui_value(type)}."
        ) )
      }
    }
  )()

  status_msg(
    res,
    fail = glue::glue("Failed to retrieve repos for org {usethis::ui_value(org)}.")
  )

  if (failed(res))
    return(invisible(NULL))

  if (full_repo) {
    res = purrr::map_chr(result(res), "full_name")
  } else {
    res = purrr::map_chr(result(res), "name")
  }

  filter_results(res, filter, exclude)
}
