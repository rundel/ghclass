# Create, delete, and rename teams within an organization

- `team_create()` - create teams in a GitHub organization

- `team_delete()` - delete a team from a GitHub organization.

- `team_rename()` - rename an existing team

## Usage

``` r
team_create(
  org,
  team,
  prefix = "",
  suffix = "",
  privacy = c("secret", "closed")
)

team_delete(org, team, team_type = c("name", "slug"), prompt = TRUE)

team_rename(org, team, new_team, team_type = c("name", "slug"))
```

## Arguments

- org:

  Character. Name of the GitHub organization.

- team:

  Character. Name of teams.

- prefix:

  Character. Shared prefix.

- suffix:

  Character. Shared suffix.

- privacy:

  Character. Level of privacy for team, "closed" (visible to all members
  of the organization) or "secret" (only visible to organization owners
  and members of a team), default is "secret"

- team_type:

  Character. Either "slug" if the team names are slugs or "name" if full
  team names are provided.

- prompt:

  Logical. Should the user be prompted before deleting team. Default
  `true`.

- new_team:

  character, new team name.

## Value

All functions invisibly return a list containing the results of the
relevant GitHub API calls.

## Examples

``` r
if (FALSE) { # \dontrun{
team_create("ghclass-test",c("hw1-team01","hw1-team02"))

org_teams("ghclass-test", "hw1-")

team_rename("ghclass-test", "hw1-team02", "hw1-team03")

org_teams("ghclass-test", "hw1-")

team_delete("ghclass-test", "hw1-team01", prompt = FALSE)

org_teams("ghclass-test", "hw1-")


# Cleanup
team_delete("ghclass-test", org_teams("ghclass-test", "hw1-"), prompt = FALSE)
} # }
```
