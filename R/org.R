#' @name org_perm
#' @rdname org_perm
#'
#' @title Organization permissions
#'
#' @description
#'
#' * `org_sitrep` - Provides a situation report on a GitHub organization.
#'
#' * `org_set_repo_permission` - Change the default permission level for org repositories.
#'
#' @param org Character. Name of the GitHub organization(s).
#' @param permission Default permission level members have for organization repositories:
#' * read - can pull, but not push to or administer this repository.
#' * write - can pull and push, but not administer this repository.
#' * admin - can pull, push, and administer this repository.
#' * none - no permissions granted by default.
#'
#' @examples
#' \dontrun{
#' org_sitrep("ghclass-test")
#'
#' org_set_repo_permission("ghclass-test", "read")
#'
#' org_sitrep("ghclass-test")
#'
#' # Cleanup
#' org_set_repo_permission("ghclass-test", "none")
#' }
#'
NULL

#' @name org_members
#' @rdname org_members
#'
#' @title Tools for managing organization membership
#'
#' @description
#'
#' * `org_invite` - invites user(s) to a GitHub organization.
#'
#' * `org_remove` - remove user(s) from an organization (and all teams within that organization).
#'
#' * `org_members` - returns a (filtered) vector of organization members.
#'
#' * `org_pending` - returns a (filtered) vector of pending organization members.
#'
#' * `org_admins` - returns a vector of repository administrators. In the case of a
#' non-organization owner (e.g. a user account) returns the owner's login.
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
#'
#' org_admins("rundel") # User, not an organization
#'
#'
#'# Org Membership - Invite, Status, and Remove
#' students = c("ghclass-anya", "ghclass-bruno", "ghclass-celine",
#'              "ghclass-diego", "ghclass-elijah","ghclass-francis")
#'
#' org_invite("ghclass-test", students)
#'
#' org_members("ghclass-test")
#'
#' org_pending("ghclass-test")
#'
#' org_remove("ghclass-test", students, prompt = FALSE)
#'
#' org_pending("ghclass-test")
#' }
#'

NULL




#' @name org_details
#' @rdname org_details
#'
#' @title Obtain details on an organization's repos and teams
#'
#' @description
#'
#' * `org_exists` - returns `TRUE` if the organization(s) exist on GitHub and `FALSE` otherwise.
#'
#' * `org_teams` - returns a (filtered) vector of organization teams.
#'
#' * `org_team_details` - returns a data frame of all organization teams containing identification and permission details.
#'
#' * `org_repos` - returns a (filtered) vector of organization repositories.
#'
#' * `org_repo_stats` - returns a tibble of repositories belonging to a GitHub organization along with some
#' basic statistics about those repositories.
#'
#' @param org Character. Name of the GitHub organization(s).
#' @param filter Character. Regular expression pattern for matching (or excluding) results
#' @param exclude Logical. Should entries matching the regular expression be excluded or included.
#'
#' @examples
#' \dontrun{
#' # Org repos and teams
#' org_repos("ghclass-test")
#'
#' org_repos("ghclass-test", filter = "hw1-")
#'
#' org_teams("ghclass-test")
#'
#' org_team_details("ghclass-test")
#' }
#'

NULL

