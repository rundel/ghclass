#' @rdname org_perm
#' @export
#'
org_allows_forking = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org)(org)

  if (failed(res)) {
    cli::cli_abort("Failed to find org {.val {org}}.")
  }

  result(res)$members_can_fork_private_repositories
}
