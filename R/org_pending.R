github_api_org_pending = function(owner){
  arg_is_chr_scalar(owner)
  gh::gh("GET /orgs/:owner/invitations",
         owner = owner,
         .token = github_get_token(),
         .limit = github_get_api_limit())
}

#' @rdname org
#' @export
#'
org_pending = function(org, filter = NULL, exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_org_pending)(org)
  status_msg(
    res,
    fail = "Failed to retrieve pending members for org {.val {org}}"
  )

  invite = purrr::map(result(res), "login")
  invite = purrr::flatten_chr(invite)
  filter_results(invite, filter, exclude)
}
