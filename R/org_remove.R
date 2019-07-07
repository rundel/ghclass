github_api_org_remove = function(org, user) {
  gh::gh(
    "DELETE /orgs/:org/members/:username",
    org = org,
    username = user,
    .token = github_get_token()
  )
}

#' Remove a member from an organization
#'
#' `org_remove` removes a user from the organization and all teams within that organzation.
#'
#' @param org Character. Name of the GitHub organization.
#' @param user Character. Name of one or more GitHub users.
#' @param prompt Logical. Should the user be prompted before deleting repositories. Default `true`.
#'

#' @export
#'
org_remove = function(org, user, prompt = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(user, allow_null = TRUE)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = usethis::ui_yeah( paste(
      "This command will delete the following users:",
      "{usethis::ui_value(user)} from org {usethis::ui_value(org)}."
    ) )
    if (!delete) {
      return(invisible())
    }
  }

  purrr::walk(
    user,
    function(user) {
      res = purrr::safely(github_api_org_remove)(org, user)

      status_msg(
        res,
        glue::glue("Removed user {usethis::ui_value(user)} from org {usethis::ui_value(org)}."),
        glue::glue("Failed to remove user {usethis::ui_value(user)} from org {usethis::ui_value(org)}.")
      )
    }
  )
}
