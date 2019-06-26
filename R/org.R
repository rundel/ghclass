github_api_get_repos = function(owner) {
  stopifnot(length(owner) == 1)
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

  stopifnot(length(org) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_repos(org)
  res = purrr::map_chr(res, "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}


github_api_get_member = function(owner) {
  stopifnot(length(owner) == 1)
  safe_gh("GET /orgs/:owner/members",
          owner = owner,
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

  stopifnot(length(org) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_member(org)
  member = purrr::map_chr(res$result, "login")

  filter_results(member, filter, exclude)
}



github_api_get_invitation = function(owner){

  safe_gh("GET /orgs/:owner/invitations",
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

  stopifnot(length(org) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_invitation(org)
  invite = purrr::map_chr(res$result, "login")

  filter_results(invite, filter, exclude)
}

github_api_get_user = function(user)
{
  safe_gh(
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
check_user_exists = function(user)
{
  res = purrr::map(user, github_api_get_user)
  purrr::map_lgl(res, succeeded)
}


github_api_invite_user = function(org, user) {
  stopifnot(length(org) == 1)
  stopifnot(length(user) == 1)
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

  stopifnot(length(org) == 1)

  user = tolower(user)
  member = tolower(get_member(org))
  pending = tolower(get_pending_member(org))

  need_invite = setdiff(user, c(member, pending))
  is_member = intersect(user, member)
  is_pending = intersect(user, pending)

  if(length(need_invite) > 0){
    purrr::walk(
      need_invite,
      function(need_invite) {
        res = purrr::safely(github_api_invite_user)(org, need_invite)

        status_msg(
          res,
          usethis::ui_done("Added user {usethis::ui_value(need_invite)} to org {usethis::ui_value(org)}."),
          usethis::ui_oops("Failed to add user {usethis::ui_value(need_invite)} to org {usethis::ui_value(org)}: does not exist.")
        )
      }
    )
  }

  if(length(is_member) > 0){
    purrr::walk(
      is_member,
      function(is_member)
        usethis::ui_oops("{usethis::ui_value(is_member)} already member of org {usethis::ui_value(org)}.")
    )
  }

  if(length(is_pending) > 0){
    purrr::walk(
      is_pending,
      function(is_pending)
        usethis::ui_oops("{usethis::ui_value(is_pending)} is a pending member of org {usethis::ui_value(org)}.")
    )
  }
}

