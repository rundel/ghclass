# Tools for inviting, removing, and managing members of an organization team

- `team_invite()` - add members to team(s).

- `team_remove()` - remove members from team(s).

- `team_members()` - returns a tibble of team members.

- `team_pending()` - returns a tibble of pending team members.

- `team_repos()` - returns a tibble of teams and their repos.

## Usage

``` r
team_invite(org, user, team, team_type = c("name", "slug"))

team_members(org, team = org_teams(org), team_type = c("name", "slug"))

team_pending(org, team = org_teams(org), team_type = c("name", "slug"))

team_remove(org, user, team, team_type = c("name", "slug"))

team_repos(org, team = org_teams(org), team_type = c("name", "slug"))
```

## Arguments

- org:

  Character. Name of the GitHub organization.

- user:

  Character. One or more GitHub users to invite.

- team:

  Character. Name of teams.

- team_type:

  Character. Either "slug" if the team names are slugs or "name" if full
  team names are provided.

## Value

`team_members()`, `team_pending()`, and `team_repos()` all return a
tibble. The `team` column echoes the supplied `team` values (names or
slugs, per `team_type`) and a `slug` column gives the corresponding team
slug used for the API calls.

`team_invite()` and `team_remove()` invisibly return a list containing
the results of the relevant GitHub API calls.

## Examples

``` r
if (FALSE) { # \dontrun{
team_create("ghclass-test",c("hw1-team01","hw1-team02"))

team_invite("ghclass-test", user = "rundel", team = c("hw1-team01", "hw1-team02", "missing_team"))

team_remove("ghclass-test", user = "rundel", team = c("hw1-team01", "missing_team"))

team_members("ghclass-test", org_teams("ghclass-test", "hw1-"))

team_pending("ghclass-test", org_teams("ghclass-test", "hw1-"))

# Add team repo
repo_create("ghclass-test", name = "hw1-team02")
repo_add_team("ghclass-test/hw1-team02", team = "hw1-team02")

team_repos("ghclass-test", org_teams("ghclass-test", "hw1-"))


# Cleanup
repo_delete("ghclass-test/hw1-team02", prompt = FALSE)
team_delete("ghclass-test", org_teams("ghclass-test", "hw1-"), prompt = FALSE)
} # }
```
