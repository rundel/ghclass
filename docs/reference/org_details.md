# Obtain details on an organization's repos and teams

- `org_exists()` - returns `TRUE` if the organization(s) exist on GitHub
  and `FALSE` otherwise.

- `org_teams()` - returns a (filtered) vector of organization teams.

- `org_team_details()` - returns a data frame of all organization teams
  containing identification and permission details.

- `org_repos()` - returns a (filtered) vector of organization
  repositories.

- `org_repo_search()` - search for repositories within an organization
  (preferred for large organizations).

- `org_repo_stats()` - returns a tibble of repositories belonging to a
  GitHub organization along with some basic statistics about those
  repositories.

- `org_repo_forking()` - returns a tibble of private repositories in an
  organization and whether forking is enabled for each.

- `org_user_repos()` - constructs a character vector of per-user repo
  addresses (`org/prefix<user>suffix`) aligned with `user`, for safely
  pairing with
  [`repo_add_user()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  and similar.

## Usage

``` r
org_exists(org)

org_repo_forking(org)

org_repo_search(org, name, extra = "", full_repo = TRUE)

org_repo_stats(
  org,
  branch = NULL,
  filter = "",
  filter_type = "in:name",
  inc_commits = TRUE,
  inc_issues = TRUE,
  inc_prs = TRUE
)

org_repos(
  org,
  filter = NULL,
  exclude = FALSE,
  full_repo = TRUE,
  sort = c("full_name", "created", "updated", "pushed"),
  direction = c("asc", "desc"),
  type = c("all", "public", "private", "forks", "sources", "member", "internal")
)

org_team_details(org)

org_teams(org, filter = NULL, exclude = FALSE, team_type = c("name", "slug"))

org_user_repos(org, user, prefix = "", suffix = "")
```

## Arguments

- org:

  Character. Name of the GitHub organization(s).

- name:

  Character. Full or partial repo name to search for within the org

- extra:

  Character. Any additional search qualifiers, see [Searching for
  repositories](https://docs.github.com/en/free-pro-team@latest/github/searching-for-information-on-github/searching-for-repositories)
  for details.

- full_repo:

  Logical. Should the full repository address be returned (e.g.
  `owner/repo` instead of just `repo`). Default `TRUE`.

- branch:

  Character. The branch to use for counting commits, if `NULL` then each
  repo's default branch is used.

- filter:

  Character. Regular expression pattern for matching (or excluding)
  results

- filter_type:

  Character. One or more GitHub search `in` qualifiers. See
  [documentation](https://help.github.com/en/articles/searching-for-repositories)
  for more details.

- inc_commits:

  Logical. Include commit statistics (`branch`, `commits`,
  `last_update`). Default `TRUE`.

- inc_issues:

  Logical. Include issue statistics (`open_issues`, `closed_issues`).
  Default `TRUE`.

- inc_prs:

  Logical. Include pull request statistics (`open_prs`, `merged_prs`,
  `closed_prs`). Default `TRUE`.

- exclude:

  Logical. Should entries matching the regular expression be excluded or
  included. Default `FALSE`.

- sort:

  Character. Sorting criteria to use, can be one of "created",
  "updated", "pushed", or "full_name".

- direction:

  Character. Sorting order to use.

- type:

  Character. Specifies the type of repositories you want, can be one of
  "all", "public", "private", "forks", "sources", "member", or
  "internal".

- team_type:

  Character. Either "slug" if the team names are slugs or "name" if full
  team names are provided.

- user:

  Character. One or more GitHub usernames.

- prefix:

  Character. String to prepend to each user name.

- suffix:

  Character. String to append to each user name.

## Value

`org_exists()` returns a logical vector.

`org_teams()`, `org_repos`, `org_repo_search()`, and `org_user_repos()`
return a character vector.

`org_team_details()`, `org_repo_stats()`, and `org_repo_forking()`
return tibbles.

## Note

The order of `org_repos()` is not guaranteed to match the order of
[`org_members()`](https://rundel.github.io/ghclass/reference/org_members.md),
so do not rely on positional pairing of the two when calling functions
like
[`repo_add_user()`](https://rundel.github.io/ghclass/reference/repo_user.md).
To pair each user with their own repo deterministically, build the repo
names from the user vector with `org_user_repos()`.

`org_repo_search()` and `org_repo_stats()` use GitHub's search API,
which returns at most 1000 results per query. For organizations with
more than 1000 matching repositories the results are truncated and a
warning is issued; use `filter` (or `name` / `extra`) to narrow the
search.

## Examples

``` r
if (FALSE) { # \dontrun{
# Org repos and teams
org_repos("ghclass-test")

org_repos("ghclass-test", filter = "hw1-")

org_teams("ghclass-test")

org_team_details("ghclass-test")
} # }
```
