# Remove org and team membership
github_api_org_remove = function(org, user) {
  gh::gh(
    "DELETE /orgs/:org/members/:username",
    org = org,
    username = user,
    .token = github_get_token()
  )
}

# Only remove membership
github_api_org_remove_membership = function(org, user) {
  gh::gh(
    "DELETE /orgs/:org/memberships/:username",
    org = org,
    username = user,
    .token = github_get_token()
  )
}

# TODO - think about removing (or not) team membership

#' @rdname org
#' @param prompt Logical. Prompt before removing member from organization.
#' @export
#'
org_remove = function(org, user, prompt = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(user, allow_null = TRUE)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = cli_yeah(
      "Org {.val {org}} will have the following {cli::qty(user)} user{?s} removed: ",
      "{.val {user}}"
    )
    if (!delete) {
      return(invisible())
    }
  }

  pending = user %in% org_pending(org)

  purrr::walk2(
    user, pending,
    function(user, pending) {
      if (pending)
        res = purrr::safely(github_api_org_remove_membership)(org, user)
      else
        res = purrr::safely(github_api_org_remove)(org, user)

      type = ternary(pending, "pending user", "user")
      status_msg(
        res,
        "Removed {type} {.val {user}} from org {.val {org}}.",
        "Failed to remove {type} {.val {user}} from org {.val {org}}."
      )
    }
  )
}
