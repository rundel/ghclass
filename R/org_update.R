github_api_org_update = function(org, ..., tries = 20, delay = 3) {
  for (i in seq_len(tries)) {
    res = purrr::safely(ghclass_api_v3_req)(
      endpoint = "PATCH /orgs/:org",
      org = org,
      ...,
      .send_headers = c(Accept = "application/vnd.github.surtur-preview+json")
    )

    # GitHub applies default_repository_permission asynchronously and answers
    # further updates with a 409 until it has finished
    if (succeeded(res) || !inherits(error(res), "http_error_409") || i == tries)
      break

    Sys.sleep(delay)
  }

  if (failed(res))
    stop(error(res))

  result(res)
}

#' @rdname org_perm
#' @export
#'
org_set_repo_permission = function(org, repo_permission = c("none", "read", "write", "admin")) {
  arg_is_chr_scalar(org)
  repo_permission = match.arg(repo_permission)

  res = purrr::safely(github_api_org_update)(org, default_repository_permission = repo_permission)

  status_msg(
    res,
    "Set org {.val {org}}'s repo permissions to {.val {repo_permission}}.",
    "failed to set org {.val {org}}'s repo permissions."
  )

  invisible(result(res))
}

#' @rdname org_perm
#' @export
#'
org_set_permissions = function(
  org,
  repo_permission = NULL,
  create_repositories = NULL,
  create_public_repositories = NULL,
  create_private_repositories = NULL,
  fork_private_repositories = NULL,
  create_teams = NULL
) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(repo_permission, allow_null = TRUE)
  arg_is_lgl_scalar(
    create_repositories, create_public_repositories, create_private_repositories,
    fork_private_repositories, create_teams,
    allow_null = TRUE
  )
  if (!is.null(repo_permission))
    repo_permission = match.arg(repo_permission, c("none", "read", "write", "admin"))

  args = purrr::compact(list(
    repo_permission = repo_permission,
    create_repositories = create_repositories,
    create_public_repositories = create_public_repositories,
    create_private_repositories = create_private_repositories,
    fork_private_repositories = fork_private_repositories,
    create_teams = create_teams
  ))

  if (length(args) == 0)
    cli_stop("At least one setting must be provided.")

  api_names = c(
    repo_permission = "default_repository_permission",
    create_repositories = "members_can_create_repositories",
    create_public_repositories = "members_can_create_public_repositories",
    create_private_repositories = "members_can_create_private_repositories",
    fork_private_repositories = "members_can_fork_private_repositories",
    create_teams = "members_can_create_teams"
  )

  params = args
  names(params) = api_names[names(args)]
  changes = paste0(names(args), " = ", unlist(args))

  res = purrr::safely(do.call)(github_api_org_update, c(list(org = org), params))

  if (failed(res)) {
    status_msg(res, fail = "Failed to set org {.val {org}} permissions.")
    return(invisible(NULL))
  }

  # GitHub returns 200 even for settings it does not apply
  org_res = result(res)
  applied = purrr::map_lgl(names(params), function(f) identical(org_res[[f]], params[[f]]))

  if (any(applied))
    cli::cli_alert_success("Set org {.val {org}} permissions: {.field {changes[applied]}}.")
  if (any(!applied))
    cli::cli_alert_warning(
      "GitHub did not apply {.field {changes[!applied]}} for org {.val {org}}."
    )

  invisible(org_res)
}
