github_api_get_repos = function(org) {
  stopifnot(length(org)==1)
  gh("GET /orgs/:org/repos", org = org, .token=get_github_token(), .limit=get_github_api_limit())
}

#' @export
get_repo = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE) {
  get_repos(org, filter, exclude, full_repo)
}

#' Get organization repos
#'
#' \code{get_repos} returns a (filtered) vector of repos belonging to a GitHub organization.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#' @param full_repo logical, should the full repo name be returned (e.g. \code{org/repo} instead of just \code{repo})
#'
#' @aliases get_repo
#'
#' @examples
#' \dontrun{
#' get_repos("ghclass")
#' get_repos("ghclass", "hw1-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = purrr::map_chr(github_api_get_repos(org), "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}


github_api_get_members = function(org) {
  stopifnot(length(org)==1)
  gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_github_api_limit())
}


#' Get organization members
#'
#' \code{get_members} returns a (filtered) vector of organization memebers.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_members("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_members = function(org, filter=NULL, exclude=FALSE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = purrr::map_chr(github_api_get_members(org), "login")

  filter_results(res, filter, exclude)
}



github_api_get_invitations = function(org) {
  gh("GET /orgs/:org/invitations", org=org,
     .token=get_github_token(), .limit=get_github_api_limit())
}

#' Get pending organization members
#'
#' \code{get_pending_members} returns a (filtered) vector of pending organization memebers.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_pending_members("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_pending_members = function(org, filter=NULL, exclude=FALSE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = github_api_get_invitations(org)
  res = purrr::map(res, "login")
  res = as.character(unlist(res))

  filter_results(res, filter, exclude)
}




#' @export
check_user_exists = function(user)
{
  check_user = function(user) {
    gh("/users/:username", username=user, .token=get_github_token())
    TRUE
  }

  purrr::map_lgl(user, purrr::possibly(check_user, FALSE))
}



github_api_invite_user = function(org, user) {
  stopifnot(length(org) == 1)
  stopifnot(length(user) == 1)

  gh(
    "PUT /orgs/:org/memberships/:username",
    org=org, username=user, role="member",
    .token=get_github_token()
  )
}


#' Invite user(s)
#'
#' \code{invite_user} invites users to your organization
#'
#' @param org character, name of the GitHub organization.
#' @param user character or data frame, listing one or more users
#'
#' @examples
#' \dontrun{
#' users = c("Alice","Bob","Carol","Dave","Eve")
#' invite_user("ghclass", users)
#' }
#'
#' @family github organization related functions
#'
#' @export
invite_user = function(org, user, exclude_pending = FALSE) {
  stopifnot(length(org) == 1)

  user = tolower(user)
  members = tolower(get_members(org))
  pending = tolower(get_pending_members(org))

  need_invite = setdiff(user, c(members, pending))

  purrr::walk(
    need_invite,
    function(user) {
      res = purrr::safely(github_api_invite_user)(org, user)

      status_msg(
        res,
        glue::glue("Added user {usethis::ui_value(user)} to org {usethis::ui_value(org)}."),
        glue::glue("Failed to add user {usethis::ui_value(user)} to org {usethis::ui_value(org)}."),
      )
    }
  )
}

