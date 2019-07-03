github_api_get_repos = function(owner) {
  arg_is_chr_scalar(owner)

  gh::gh("GET /orgs/:owner/repos",
          owner = owner,
          .token = get_github_token(),
          .limit = get_github_api_limit())
}

#' Get organization repository
#'
#' `get_repo` returns a (filtered) vector of repositories belonging to a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param full_repo Logical. Should the full repository address be returned (e.g. `owner/name` instead of just `repo`)?
#'
#'
#' @examples
#' \dontrun{
#' get_repo("ghclass")
#' get_repo("ghclass", "hw1-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_repo = function(org, filter = NULL, exclude = FALSE, full_repo = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = github_api_get_repos(org)
  res = purrr::map_chr(res, "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}


github_api_get_member = function(org) {
  arg_is_chr_scalar(org)

  gh::gh("GET /orgs/:org/members",
          org = org,
          .token = get_github_token(),
          .limit = get_github_api_limit())
}


#' Get organization member
#'
#' `get_member` returns a (filtered) vector of organization memebers.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_member("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_member = function(org, filter = NULL, exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_get_member)(org)
  member = purrr::map_chr(result(res), "login")

  filter_results(member, filter, exclude)
}



github_api_get_invitations = function(owner){
  arg_is_chr_scalar(owner)
  gh::gh("GET /orgs/:owner/invitations",
         owner = owner,
         .token = get_github_token(),
         .limit = get_github_api_limit())
}


#' Get pending organization members
#'
#' `get_pending_member` returns a (filtered) vector of pending organization memebers.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repos.
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_pending_member("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_pending_member = function(org, filter = NULL, exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)

  res = purrr::safely(github_api_get_invitations)(org)
  status_msg(
    res,
    fail = glue::glue("Failed to retrieve pending members for org {usethis::ui_value(org)}")
  )

  invite = purrr::map(result(res), "login")
  invite = purrr::flatten_chr(invite)
  filter_results(invite, filter, exclude)
}

github_api_get_user = function(user) {
  arg_is_chr_scalar(user)

  gh::gh(
    "/users/:username",
    username = user,
    .token = get_github_token()
  )
}

#' Check if username(s) exists
#'
#' `check_user_exists` returns TRUE if the supplied username(s) exists on GitHub and FALSE otherwise.
#'
#' @param user Character. Username to be checked. Can be a vector or list of usernames.
#'
#' @return TRUE or FALSE
#'
#' @examples
#' \dontrun{
#' check_user_exists(c("rundel","hopefullydoesnotexist"))
#' }
#'
#' @export
#'
check_user_exists = function(user) {
  arg_is_chr(user)

  res = purrr::map(user, github_api_get_user)
  purrr::map_lgl(res, succeeded)
}


github_api_invite_user = function(org, user) {
  arg_is_chr_scalar(org, user)

  gh::gh(
    "PUT /orgs/:org/memberships/:username",
    org = org,
    username = user,
    role = "member",
    .token = get_github_token()
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


github_api_org_remove = function(org, user) {
  gh::gh("DELETE /orgs/:org/members/:username",
         org = org,
         username = user,
         .token = get_github_token())
}

#' Remove a member from an organization
#'
#' `org_remove` removes a user from the organization and all teams within that organzation.
#'
#' @param org Character. Name of the GitHub organization.
#' @param user Character. Name of one or more GitHub users.
#' @param prompt Logical. Should the user be prompted before deleting repositories. Default `true`.
#'
#' @family github organization related functions
#'
#' @export
#'
org_remove = function(org, user, prompt = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(user)
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
