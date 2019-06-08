#' @templateVar fun github_api_get_repos
#' @template template-depr_fun
#'
#' @templateVar old github_api_get_repos
#' @templateVar new github_api_get_repo
#' @template template-depr_pkg
#'
github_api_get_repos = function(org) {

  .Deprecated(msg = "'github_api_get_repos' will be removed in the next version. Use 'github_api_get_repo' instead.",
              new = "github_api_get_repo")
  stopifnot(length(org)==1)
  gh("GET /orgs/:org/repos", org = org, .token=get_github_token(), .limit=get_github_api_limit())
}


github_api_get_repo = function(org) {
  stopifnot(length(org)==1)
  safe_gh("GET /orgs/:org/repos",
          org = org,
          .token=get_github_token(),
          .limit=get_github_api_limit())
}

#'
#' @export
get_repo = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE) {
  get_repos(org, filter, exclude, full_repo)
}

#' Get organization repos
#'
#' \code{get_repos} returns a (filtered) vector of repos belonging to a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in \code{filter} be excluded or included?
#' @param full_repo Logical. Should the full repository name be returned (e.g. \code{org/repo} instead of just \code{repo})?
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

  res = purrr::map_chr(github_api_get_repo(org), "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}


github_api_get_members = function(org) {
  stopifnot(length(org)==1)
  safe_gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_github_api_limit())
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

  res = github_api_get_members(org)
  members = purrr::map_chr(res$result, "login")

  filter_results(members, filter, exclude)
}



github_api_get_invitations = function(org) {
  safe_gh("GET /orgs/:org/invitations", org=org,
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
  invites = purrr::map_chr(res$result, "login")

  filter_results(invites, filter, exclude)
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
#' \code{check_user_exists} returns TRUE if the supplied username(s) exists on GitHub and FALSE otherwise.
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


github_api_create_invitation = function(org, user){
  safe_gh(
    "PUT /orgs/:org/memberships/:username",
    org=org, username=user, role="member",
    .token=get_github_token()
  )
}


#' Invite user(s)
#'
#' \code{invite_user} invites user(s) to your organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param user Character, character vector, or list. Listing one or more user names.
#' @param exclude_pending Logical. If \code{exclude_pending = TRUE}, pending members will not receive another invitation tp join the organization. The default is FALSE, such that pending members will receive another invitation email.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' users = c("Alice","Bob","Carol","Dave","Eve")
#' invite_user("Sta523-Fa17", users)
#' }
#'
#' @family github organization related functions
#'
#' @export
invite_user = function(org, user, exclude_pending = FALSE, verbose = TRUE)
{
  stopifnot(length(org) == 1)

  user = tolower(user)
  members = tolower(get_members(org))
  pending = tolower(get_pending_members(org))

  need_invite = ifelse(exclude_pending == T,
                       setdiff(user, c(members, pending)),
                       setdiff(user, c(members)))

  purrr::walk(
    need_invite,
    function(user) {

      if (verbose)
        message("Inviting ", user, " to ", org, " ...")

      res = github_api_create_invitation(org, user)

      fail = sprintf("Inviting %s to %s failed.", user, org)
      check_result(res, fail)
    }
  )

  already_member = setdiff(user, c(members))

  purrr::walk(
    already_member,
    function(user)
      if(verbose)
        message(user, " already part of ", org, " ...")
  )
}
