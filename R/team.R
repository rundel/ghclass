#' @name team
#' @rdname team
#'
#' @title GitHub team related tools
#'
#' @description
#'
#' `team_create` - create teams in a GitHub organization
#'
#' `team_delete` - delete a team from a GitHub organization.
#'
#' `team_rename` - renames an existing team.
#'
#' `team_invite` - add members to GitHub organization team(s).
#'
#' `team_members` - returns a data frame of team members.
#'
#' `team_pending` - returns a data frame of pending team members.
#'
#' `team_repos` - returns a data frame of teams and their repos.
#'
#' @param org Character. Name of the GitHub organization.
#' @param team Character. Name of teams.
#' @param team_type Character. Either "slug" if the team names are slugs or "name" if full team names are provided.
#' @param prefix Character. Shared prefix.
#' @param suffix Character. Shared suffix.
#' @param privacy Character. Level of privacy for team, "closed" (visible to all
#' members of the organization) or "secret" (only visible to organization owners
#' and members of a team), default is "closed"
#' @param prompt Logical. Should the user be prompted before deleting team. Default `true`.
#' @param user Character. One or more GitHub usernames to invite.
#'
#' @examples
#' \dontrun{
#' team_delete("ghclass-test", org_teams("ghclass-test", "hw1-"), prompt = FALSE)
#' team_create("ghclass-test",c("hw1-team01","hw1-team02"))
#' team_rename("ghclass-test", "hw1-team02", "hw1-team03")
#' org_teams("ghclass-test", "hw1-")
#'
#'
#' team_invite("ghclass-test", user = "rundel", team = c("hw1-team01", "hw1-team03", "missing_team"))
#' team_remove("ghclass-test", user = "rundel", team = "hw1-team01")
#'
#' team_members("ghclass-test", org_teams("ghclass-test", "hw1-"))
#' team_pending("ghclass-test", org_teams("ghclass-test", "hw1-"))
#' team_repos("ghclass-test", org_teams("ghclass-test", "hw1-"))
#' }
#'
NULL
