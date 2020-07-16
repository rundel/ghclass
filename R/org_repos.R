github_api_org_repos = function(owner) {
  arg_is_chr_scalar(owner)

  ghclass_api_v3_req(
    endpoint = "GET /orgs/:owner/repos",
    owner = owner
  )
}

#' @rdname org_details
#' @param full_repo Logical. Should the full repository address be returned (e.g. `owner/repo` instead of just `repo`)?
#' @export
#'
org_repos = function(org, filter = NULL, exclude = FALSE, full_repo = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(
    function() {
      type = user_type(org)

      if (is.na(type)) {
        cli_stop("Organization {.val {org}} does not exist on GitHub.")
      } else if (type == "Organization") {
        github_api_org_repos(org)
      } else if (type == "User") {
        cli_stop("{.val {org}} is a user not an organization. Use {.fun user_repos} instead.")
      } else {
        cli_stop("{.val {org}} has unknown type {.val {type}}.")
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
