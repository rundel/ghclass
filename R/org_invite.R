github_api_org_invite = function(org, user) {
  arg_is_chr_scalar(org, user)

  gh::gh(
    "PUT /orgs/:org/memberships/:username",
    org = org,
    username = user,
    role = "member",
    .token = github_get_token()
  )
}


#' Invite user(s)
#'
#' `org_invite` invites user(s) to your organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param user Character, character vector, or list. Listing one or more user names.
#'
#' @examples
#' \dontrun{
#' user = c("Alice","Bob","Carol","Dave","Eve")
#' org_invite("Sta523-Fa17", user)
#' }
#'

#' @aliases invite_user
#'
#' @export
org_invite = function(org, user) {
  arg_is_chr_scalar(org)
  arg_is_chr(user)

  user = unique(tolower(user))
  member = tolower(org_members(org))
  pending = tolower(org_pending_members(org))

  purrr::walk(
    user,
    function(user) {
      if (user %in% member) {
        usethis::ui_info("User {usethis::ui_value(user)} is already a member of org {usethis::ui_value(org)}.")
      } else if (user %in% pending) {
        usethis::ui_info("User {usethis::ui_value(user)} is already a pending member of org {usethis::ui_value(org)}.")
      } else {
        res = purrr::safely(github_api_org_invite)(org, user)

        status_msg(
          res,
          glue::glue("Invited user {usethis::ui_value(user)} to org {usethis::ui_value(org)}."),
          glue::glue("Failed to invite user {usethis::ui_value(user)} to org {usethis::ui_value(org)}: does not exist.")
        )
      }
    }
  )
}
