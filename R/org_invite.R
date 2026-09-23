github_api_org_invite = function(org, user) {
  arg_is_chr_scalar(org, user)

  ghclass_api_v3_req(
    endpoint = "PUT /orgs/:org/memberships/:username",
    org = org,
    username = user,
    role = "member"
  )
}


#' @rdname org_members
#' @export
#'
org_invite = function(org, user) {
  arg_is_chr_scalar(org)
  arg_is_chr(user)

  user = unique(tolower(user))
  member = tolower(org_members(org))
  pending = tolower(org_pending(org))

  res = status_scope(
    "Inviting users", length(user),
    done = "Invited {n_ok} of {total} user{?s} to org {.val {org}}",
    purrr::map(
      user,
      function(user) {
        if (user %in% member) {
          status_skip("User {.val {user}} is already a member of org {.val {org}}.")
        } else if (user %in% pending) {
          status_skip("User {.val {user}} is already a pending member of org {.val {org}}.")
        } else {
          res = purrr::safely(github_api_org_invite)(org, user)

          status_msg(
            res,
            "Invited user {.val {user}} to org {.val {org}}.",
            "Failed to invite user {.val {user}} to org {.val {org}}."
          )
        }
      }
    )
  )

  invisible(res)
}
