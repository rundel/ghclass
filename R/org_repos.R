github_api_org_repos = function(org, type = NULL, sort = NULL, direction = NULL) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(type, sort, direction, allow_null = TRUE)

  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/repos",
    org = org,
    type = type,
    sort = sort,
    direction = direction
  )
}

#' @rdname org_details
#' @param full_repo Logical. Should the full repository address be returned (e.g. `owner/repo` instead of just `repo`).
#' @param sort Character. Sorting criteria to use, can be one of "created", "updated", "pushed", or "full_name".
#' @param direction Character. Sorting order to use.
#' @param type  Character. Specifies the type of repositories you want, can be one of
#' "all", "public", "private", "forks", "sources", "member", or "internal".
#'
#' @export
#'
org_repos = function(
  org, filter = NULL, exclude = FALSE, full_repo = TRUE,
  sort = c("full_name", "created", "updated", "pushed"),
  direction = c("asc", "desc"),
  type = c("all", "public", "private", "forks", "sources", "member", "internal")
) {
  sort = match.arg(sort)
  direction = match.arg(direction)
  type = match.arg(type)

  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl_scalar(exclude, full_repo)
  arg_is_chr_scalar(sort, direction, type)

  res = purrr::safely(
    function() {
      org_type = user_type(org)

      if (is.na(org_type)) {
        cli_stop("Organization {.val {org}} does not exist on GitHub.")
      } else if (org_type == "Organization") {
        github_api_org_repos(org, type = type, sort = sort, direction = direction)
      } else if (org_type == "User") {
        cli_stop("{.val {org}} is a user not an organization. Use {.fun user_repos} instead.")
      } else {
        cli_stop("{.val {org}} has unknown type {.val {org_type}}.")
      }
    }
  )()

  status_msg(
    res,
    fail = "Failed to retrieve repos for org {.val {org}}."
  )

  if (failed(res) | empty_result(res))
    return(invisible(NULL))

  if (full_repo) {
    res = purrr::map_chr(result(res), "full_name")
  } else {
    res = purrr::map_chr(result(res), "name")
  }

  filter_results(res, filter, exclude)
}
