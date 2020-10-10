github_api_repo_search = function(q, sort = NULL, order = c("desc", "asc")) {
  if (!is.null(sort))
    stopifnot(sort %in% "stars", "forks", "help-wanted-issues", "updated")

  order = match.arg(order)

  arg_is_chr_scalar(q)
  arg_is_chr_scalar(sort, order, allow_null = TRUE)

  ghclass_api_v3_req(
    endpoint = "GET /search/repositories",
    q = q,
    sort = sort,
    order = order
  )
}

#' @rdname org_details
#' @param name Character. Full or partial repo name to search for within the org
#' @param extra Character. Any additional search qualifiers, see
#' [Searching for repositories](https://docs.github.com/en/free-pro-team@latest/github/searching-for-information-on-github/searching-for-repositories)
#' for details.
#' @export
#'
org_repo_search = function(org, name, extra = "", full_repo = TRUE) {
  arg_is_chr_scalar(org, name, extra)
  arg_is_lgl_scalar(full_repo)

  q = glue::glue("org:{org} {name} in:name {extra}")

  res = purrr::safely(github_api_repo_search)(q)


  status_msg(
    res,
    fail = "Repo search failed for org {.val {org}}."
  )

  if (failed(res) | empty_result(res))
    return(character())

  if (full_repo) {
    purrr::map_chr(result(res)[["items"]], "full_name")
  } else {
    purrr::map_chr(result(res)[["items"]], "name")
  }
}
