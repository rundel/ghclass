github_api_get_repos = function(owner) {
  arg_is_chr_scalar(owner)

  gh::gh("GET /orgs/:owner/repos",
         owner = owner,
         .token = github_get_token(),
         .limit = get_github_api_limit())
}

#' Get organization repository
#'
#' `get_repo` returns a (filtered) vector of repositories belonging to a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param full_repo Logical. Should the full repository address be returned (e.g. `owner/name` instead of just `repo`)?
#'
#'
#' @examples
#' \dontrun{
#' get_repo("ghclass")
#' get_repo("ghclass", "hw1-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_repo = function(org, filter = NULL, exclude = FALSE, full_repo = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = github_api_get_repos(org)
  res = purrr::map_chr(res, "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}
