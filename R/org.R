#' @name org
#' @rdname org
#'
#' @title GitHub Organization related tools
#'
#' @description
#
#' `org_admins` - returns a vector of repository administrators. In the case of a
#' non-organization owner (e.g. a user account) returns the owner's login.
#'
#' `org_exists` - returns `TRUE` if the organization(s) exist on GitHub and `FALSE` otherwise.
#'
#' `org_invite` - invites user(s) to a GitHub organization.
#'
#' `org_remove` - remove user(s) from an organization (and all teams within that organization).
#'
#' `org_members` - returns a (filtered) vector of organization members.
#'
#' `org_pending` - returns a (filtered) vector of pending organization members.
#'
#' `org_repos` - returns a (filtered) vector of organization repositories.
#'
#' `org_teams` - returns a (filtered) vector of organization teams.
#'
#' @param org Character. Name of the GitHub organization.
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#'
#'
#' @param org Character. Name of the GitHub organization(s).
#' @param user Character. GitHub username(s).
#' @param filter Character. Regular expression pattern for matching (or excluding) results
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#'
#' @examples
#' \dontrun{
#' # Org Details
#' org_admins("ghclass-test")
#' org_admins("rundel") # User, not an organization
#'
#' org_exists(c("rundel","ghclass-test"))
#'
#'# Org Membership - Invite, Status, and Remove
#' students = c("ghclass-anya", "ghclass-bruno", "ghclass-celine",
#'          "ghclass-diego", "ghclass-elijah","ghclass-francis")
#' org_invite(students)
#'
#' org_members("ghclass-test")
#' org_pending("ghclass-test")
#'
#' org_remove("ghclass-test", students)
#' org_pending("ghclass-test")
#'
#' # Org repos and teams
#' org_repos("ghclass-test")
#' org_repos("ghclass-test", "hw1-")
#' org_teams("ghclass-test")
#' }
#'
#'
#' @details
#'
#'
#' @seealso [org_repo_stats]



NULL
