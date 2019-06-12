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


github_api_get_repo = function(owner) {
  stopifnot(length(owner) == 1)
  safe_gh("GET /orgs/:owner/repos",
          owner = owner,
          .token = get_github_token(),
          .limit = get_github_api_limit())
}


#' Get organization repos
#'
#' `get_repos` returns a (filtered) vector of repos belonging to a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param full_repo Logical. Should the full repository name be returned (e.g. `org/repo` instead of just `repo`)?
#'
#' @templateVar fun get_repos
#' @template template-depr_fun
#'
#' @templateVar old get_repos
#' @templateVar new get_repo
#' @template template-depr_pkg
#'
#' @examples
#' \dontrun{
#' get_repos("ghclass")
#' get_repos("ghclass", "hw1-")
#' }
#'
#' @family github organization related functions
#'
#'
get_repos = function(org, filter = NULL, exclude = FALSE, full_repo = TRUE) {

  .Deprecated(msg = "'get_repos' will be removed in the next version. Use 'get_repo' instead.",
              new = "get_repo")

  stopifnot(length(org) == 1)
  stopifnot(length(filter) <= 1)

  res = purrr::map_chr(github_api_get_repos(org), "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}


#' Get organization repository
#'
#' `get_repo` returns a (filtered) vector of repositories belonging to a GitHub organization.
#'
#' @param owner Character. Name of the GitHub organization.
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
get_repo = function(owner, filter = NULL, exclude = FALSE, full_repo = TRUE) {

  stopifnot(length(owner) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_repo(owner)
  res = purrr::map_chr(res$result, "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(owner,"/",res)

  res
}

github_api_get_members = function(org) {

  .Deprecated(msg = "'github_api_get_members' will be removed in the next version. Use 'github_api_get_member' instead.",
              new = "github_api_get_member")

  stopifnot(length(org) == 1)
  safe_gh("GET /orgs/:org/members", org = org, .token = get_github_token(), .limit = get_github_api_limit())
}

github_api_get_member = function(owner) {
  stopifnot(length(owner) == 1)
  safe_gh("GET /orgs/:owner/members",
          owner = owner,
          .token = get_github_token(),
          .limit = get_github_api_limit())
}


#' Get organization members
#'
#' `get_members` returns a (filtered) vector of organization memebers.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @templateVar fun get_members
#' @template template-depr_fun
#'
#' @templateVar old get_members
#' @templateVar new get_member
#' @template template-depr_pkg
#'
#' @examples
#' \dontrun{
#' get_members("ghclass")
#' }
#'
#' @family github organization related functions
#'
get_members = function(org, filter = NULL, exclude = FALSE) {

  .Deprecated(msg = "'get_members' will be removed in the next version. Use 'get_member' instead.",
              new = "get_member")

  stopifnot(length(org) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_members(org)
  members = purrr::map_chr(res$result, "login")

  filter_results(members, filter, exclude)
}


#' Get organization members
#'
#' `get_member` returns a (filtered) vector of organization memebers.
#'
#' @param owner Character. Name of the GitHub organization.
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
get_member = function(owner, filter = NULL, exclude = FALSE) {

  stopifnot(length(owner) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_member(owner)
  member = purrr::map_chr(res$result, "login")

  filter_results(member, filter, exclude)
}


github_api_get_invitations = function(org) {

  .Deprecated(msg = "'github_api_get_invitations' will be removed in the next version. Use 'github_api_get_invitation' instead.",
              new = "github_api_get_invitation")

  safe_gh("GET /orgs/:org/invitations", org = org,
     .token = get_github_token(), .limit = get_github_api_limit())
}

github_api_get_invitation = function(owner){

  safe_gh("GET /orgs/:owner/invitations",
          owner = owner,
          .token = get_github_token(),
          .limit = get_github_api_limit())
}


#' Get pending organization members
#'
#' `get_pending_members` returns a (filtered) vector of pending organization memebers.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @templateVar fun get_pending_members
#' @template template-depr_fun
#'
#' @templateVar old get_pending_members
#' @templateVar new get_pending_member
#' @template template-depr_pkg
#'
#' @examples
#' \dontrun{
#' get_pending_members("ghclass")
#' }
#'
#' @family github organization related functions
#'
get_pending_members = function(org, filter = NULL, exclude = FALSE) {

  .Deprecated(msg = "'get_pending_members' will be removed in the next version. Use 'get_pending_member' instead.",
              new = "get_pending_member")

  stopifnot(length(org) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_invitations(org)
  invites = purrr::map_chr(res$result, "login")

  filter_results(invites, filter, exclude)
}

#' Get pending organization members
#'
#' `get_pending_member` returns a (filtered) vector of pending organization memebers.
#'
#' @param owner Character. Name of the GitHub organization.
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
get_pending_member = function(owner, filter = NULL, exclude = FALSE) {

  stopifnot(length(owner) == 1)
  stopifnot(length(filter) <= 1)

  res = github_api_get_invitation(owner)
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


github_api_create_invitation = function(owner, user){
  safe_gh(
    "PUT /orgs/:owner/memberships/:username",
    owner = owner,
    username = user,
    role = "member",
    .token = get_github_token()
  )
}


#' Invite user(s)
#'
#' `invite_user` invites user(s) to your organization.
#'
#' @param owner Character. Name of the GitHub organization.
#' @param user Character, character vector, or list. Listing one or more user names.
#' @param exclude_pending Logical. If `exclude_pending = FALSE`, pending members will  receive another invitation to join the organization. The default is FALSE, such that pending members will receive another invitation email.
#' @param verbose Logical. Display verbose output.
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
invite_user = function(owner, user, exclude_pending = FALSE, verbose = TRUE)
{
  stopifnot(length(owner) == 1)

  user = tolower(user)
  member = tolower(get_member(owner))
  pending = tolower(get_pending_member(owner))

  need_invite = ifelse(exclude_pending,
                       setdiff(user, c(member, pending)),
                       setdiff(user, c(member)))

  purrr::walk(
    need_invite,
    function(user) {

      if (verbose)
        message("Inviting ", user, " to ", owner, " ...")

      res = github_api_create_invitation(owner, user)

      fail = sprintf("Inviting %s to %s failed.", user, owner)
      check_result(res, fail)
    }
  )

  already_member = setdiff(user, c(member))

  purrr::walk(
    already_member,
    function(user)
      if(verbose)
        message(user, " already part of ", owner, " ...")
  )
}
