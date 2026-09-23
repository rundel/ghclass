# Add or remove GitHub Actions badges from a repository

- `action_add_badge()` - Add a GitHub Actions badge to a file.

- `action_remove_badge()` - Remove one or more GitHub Action badges from
  a file.

## Usage

``` r
action_add_badge(
  repo,
  workflow = NULL,
  where = "^.",
  line_padding = "\n\n\n",
  file = "README.md"
)

action_remove_badge(repo, workflow_pat = ".*?", file = "README.md")
```

## Arguments

- repo:

  Character. Address of repository in `owner/name` format.

- workflow:

  Character. Name of the workflow.

- where:

  Character. Regex pattern indicating where to insert the badge,
  defaults to the beginning of the target file.

- line_padding:

  Character. What text should be added after the badge.

- file:

  Character. Target file to be modified, defaults to `README.md`.#'

- workflow_pat:

  Character. Name of the workflow to be removed, or a regex pattern that
  matches the workflow name.

## Value

Both `action_add_badge()` and `action_remove_badge()` invisibly return a
list containing the results of the relevant GitHub API call.

## Note

`action_add_badge()` is not idempotent; re-running it adds another badge
block, so use `action_remove_badge()` first if you need to avoid
duplicates.

## Examples

``` r
if (FALSE) { # \dontrun{
action_add_badge("ghclass-test/hw1-team01")

action_remove_badge("ghclass-test/hw1-team01")
} # }
```
