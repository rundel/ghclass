# GitHub Repository tools - repository details

- `repo_clone_url()` - Returns the url, for cloning, a GitHub repo
  (either ssh or https)

- `repo_branches()` - Returns a (filtered) vector of branch names.

- `repo_commits()` - Returns a tibble of commits to a GitHub repository.

- `repo_issues()` - Returns a tibble of issues for a GitHub repository.

- `repo_n_commits()` - Returns a tibble of the number of commits in a
  GitHub repository (and branch).

- `repo_prs()` - Returns a tibble of pull requests for a GitHub
  repository.

- `repo_pushes()` - Returns a tibble of push activity to a GitHub
  repository.

## Usage

``` r
repo_branches(repo)

repo_clone_url(repo, type = c("https", "ssh"))

repo_commits(
  repo,
  branch = NULL,
  sha = branch,
  path = NULL,
  author = NULL,
  since = NULL,
  until = NULL,
  quiet = FALSE
)

repo_issues(
  repo,
  state = c("open", "closed", "all"),
  assignee = NULL,
  creator = NULL,
  mentioned = NULL,
  labels = NULL,
  sort = c("created", "updated", "comments"),
  direction = c("desc", "asc"),
  since = NULL
)

repo_n_commits(repo, quiet = FALSE)

repo_prs(repo, state = c("open", "closed", "all"), quiet = FALSE)

repo_pushes(
  repo,
  branch = NULL,
  author = NULL,
  time_period = c("all time", "day", "week", "month", "quarter", "year"),
  quiet = FALSE
)
```

## Arguments

- repo:

  Character. Address of repository in `owner/name` format.

- type:

  Character. Clone url type, either "https" or "ssh".

- branch:

  Character. Branch to list commits from.

- sha:

  Character. SHA to start listing commits from.

- path:

  Character. Only commits containing this file path will be returned.

- author:

  Character. GitHub login or email address by which to filter commit
  author.

- since:

  Character. Only issues updated at or after this time are returned.

- until:

  Character. Only commits before this date will be returned, expects
  `YYYY-MM-DDTHH:MM:SSZ` format. Filtered against the `committer_date`
  column, see Details.

- quiet:

  Logical. Should an error message be printed if a repo does not exist.
  Default `FALSE`.

- state:

  Character. Pull request state.

- assignee:

  Character. Return issues assigned to a particular username. Pass in
  "none" for issues with no assigned user, and "\*" for issues assigned
  to any user.

- creator:

  Character. Return issues created the by the given username.

- mentioned:

  Character. Return issues that mentioned the given username.

- labels:

  Character. Return issues labeled with one or more of of the given
  label names.

- sort:

  Character. What to sort results by. Can be either "created",
  "updated", or "comments".

- direction:

  Character. The direction of the sort. Can be either "asc" or "desc".

- time_period:

  Character. The time period to filter by. Options are "all time",
  "day", "week", "month", "quarter", "year".

## Value

`repo_clone_url()` and `repo_branches()` both return a character vector.

`repo_commits()`, `repo_issues()`, `repo_n_commits()`, and `repo_prs()`
all return a tibble.

## Details

For `repo_commits()`, GitHub returns two timestamps for each commit: the
author date (`date`) and the committer date (`committer_date`). These
differ for rebased, cherry-picked, or amended commits. The `since` and
`until` arguments are filtered by GitHub against the committer date.

The `login` column is `NA` when the commit's author email is not linked
to a GitHub account. The git author's `name` and `email` are still
populated in those cases.

## Examples

``` r
if (FALSE) { # \dontrun{
repo_clone_url("rundel/ghclass")

repo_branches("rundel/ghclass")

repo_commits("rundel/ghclass")

repo_issues("rundel/ghclass")

repo_n_commits("rundel/ghclass", branch = "master")

repo_prs("rundel/ghclass")
} # }
```
