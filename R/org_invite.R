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

  purrr::walk(
    user,
    function(user) {
      if (user %in% member) {
        cli::cli_alert_info("User {.val {user}} is already a member of org {.val {org}}.")
      } else if (user %in% pending) {
        cli::cli_alert_info("User {.val {user}} is already a pending member of org {.val {org}}.")
      } else {
        res = purrr::safely(github_api_org_invite)(org, user)

        status_msg(
          res,
          "Invited user {.val {user}} to org {.val {org}}.",
          "Failed to invite user {.val {user}} to org {.val {org}}: does not exist."
        )
      }
    }
  )
}
