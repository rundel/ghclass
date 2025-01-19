github_api_org_workflow_permissions = function(org) {
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/actions/permissions/workflow",
    org = org,
    .send_headers = c(Accept = "application/vnd.github+json")
  )
}

#' @rdname org_perm
#' @export
#'
org_workflow_permissions = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org_workflow_permissions)(org)

  if (failed(res)) {
    cli::cli_abort("Failed to find org {.val org}.")
  }

  result(res)$default_workflow_permissions
}


github_api_org_set_workflow_permissions = function(org, default_workflow_permissions = "read", can_approve_pull_request_reviews = FALSE) {
  ghclass_api_v3_req(
    endpoint = "PUT /orgs/:org/actions/permissions/workflow",
    org = org,
    default_workflow_permissions = default_workflow_permissions,
    can_approve_pull_request_reviews = can_approve_pull_request_reviews,
    .send_headers = c(Accept = "application/vnd.github+json")
  )
}

#' @rdname org_perm
#' @export
#'
org_set_workflow_permissions = function(org, workflow_permission = c("read","write")) {
  workflow_permission = match.arg(workflow_permission)
  arg_is_chr_scalar(org, workflow_permission)

  res = purrr::safely(github_api_org_set_workflow_permissions)(
    org, default_workflow_permissions = workflow_permission
  )

  status_msg(
    res,
    "Set org {.val {org}}'s default workflow permissions to {.val {workflow_permission}}.",
    "Failed to set org {.val {org}}'s default workflow permissions."
  )

  invisible(result(res))
}
