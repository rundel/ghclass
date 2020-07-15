#' @rdname org_details
#' @export
#'
org_team_details = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org_teams)(org)

  status_msg(
    res,
    fail = "Failed to retrieve teams for org {.val {org}}."
  )

  if (failed(res) | empty_result(res)) {
    tibble::tibble(
      slug = character(),
      id   = integer(),
      name = character(),
      privacy = character(),
      permission = character()
    )
  } else {
    r = result(res)
    tibble::tibble(
      slug       = purrr::map_chr(r, "slug"),
      id         = purrr::map_int(r, "id"),
      name       = purrr::map_chr(r, "name"),
      privacy    = purrr::map_chr(r, "privacy"),
      permission = purrr::map_chr(r, "permission")
    )
  }
}
