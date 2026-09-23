# GitHub Repository tools - user functions

- `repo_add_user()` - Add a user to a repository

- `repo_remove_user()` - Remove a user from a repository

- `repo_add_team()` - Add a team to a repository

- `repo_remove_team()` - Remove a team from a repository

- `repo_user_permission()` - Change a collaborator's permissions for a
  repository

- `repo_team_permission()` - Change a team's permissions for a
  repository

- `repo_collaborators()` - Returns a data frame of repos, their
  collaborators, and their permissions.

- `repo_contributors()` - Returns a data frame containing details on
  repository contributor(s).

## Usage

``` r
repo_add_team(
  repo,
  team,
  permission = c("push", "pull", "admin", "maintain", "triage"),
  team_type = c("name", "slug")
)

repo_team_permission(
  repo,
  team,
  permission = c("push", "pull", "admin", "maintain", "triage"),
  team_type = c("name", "slug")
)

repo_add_user(
  repo,
  user,
  permission = c("push", "pull", "admin", "maintain", "triage")
)

repo_user_permission(
  repo,
  user,
  permission = c("push", "pull", "admin", "maintain", "triage")
)

repo_collaborators(repo, include_admins = TRUE)

repo_contributors(repo)

repo_remove_team(repo, team, team_type = c("name", "slug"))

repo_remove_user(repo, user)
```

## Arguments

- repo:

  Character. Address of repository in `owner/repo` format.

- team:

  Character. Slug or name of team to add.

- permission:

  Character. Permission to be granted to a user or team for repo,
  defaults to "push".

- team_type:

  Character. Either "slug" if the team names are slugs or "name" if full
  team names are provided.

- user:

  Character. One or more GitHub usernames.

- include_admins:

  Logical. If `FALSE`, user names of users with Admin rights are not
  included, defaults to `TRUE`.

## Value

`repo_collaborators()` and `repo_contributors()` return a tibble.

All other functions invisibly return a list containing the results of
the relevant GitHub API calls.

## Details

Permissions can be set to any of the following:

- `"pull"` - can pull, but not push to or administer this repository.

- `"push"` - can pull and push, but not administer this repository.

- `"admin"` - can pull, push and administer this repository.

- `"maintain"` - Recommended for project managers who need to manage the
  repository without access to sensitive or destructive actions.

- `"triage"` - Recommended for contributors who need to proactively
  manage issues and pull requests without write access.

`repo_contributors()` is built on GitHub's `/stats/contributors`
endpoint, which groups commits by GitHub user. Commits whose author
email is not linked to a GitHub account are not included in the result.

## Examples

``` r
if (FALSE) { # \dontrun{
repo = repo_create("ghclass-test", "hw1")

team_create("ghclass-test", "team_awesome")

repo_add_user(repo, "rundel")

repo_add_team(repo, "team_awesome")

repo_remove_team(repo, "team_awesome")

repo_collaborators(repo)

repo_contributors(repo)
repo_contributors("rundel/ghclass")

# Cleanup
repo_delete(repo, prompt=FALSE)
} # }
```
