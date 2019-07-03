github_api_invite_user = function(org, user) {
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
#' `invite_user` invites user(s) to your organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param user Character, character vector, or list. Listing one or more user names.
#'
#' @examples
#' \dontrun{
#' user = c("Alice","Bob","Carol","Dave","Eve")
#' invite_user("Sta523-Fa17", user)
#' }
#'
#' @family github organization related functions
#'
#' @export
invite_user = function(org, user) {
  arg_is_chr_scalar(org)
  arg_is_chr(user)

  user = unique(tolower(user))
  member = tolower(get_member(org))
  pending = tolower(get_pending_member(org))

  purrr::walk(
    user,
    function(user) {
      if (user %in% member) {
        usethis::ui_info("User {usethis::ui_value(user)} is already a member of org {usethis::ui_value(org)}.")
      } else if (user %in% pending) {
        usethis::ui_info("User {usethis::ui_value(user)} is already a pending member of org {usethis::ui_value(org)}.")
      } else {
        res = purrr::safely(github_api_invite_user)(org, user)

        status_msg(
          res,
          glue::glue("Invited user {usethis::ui_value(user)} to org {usethis::ui_value(org)}."),
          glue::glue("Failed to invite user {usethis::ui_value(user)} to org {usethis::ui_value(org)}: does not exist.")
        )
      }
    }
  )
}
