# GitHub Issue related tools

- `issue_create` creates a new issue.

- `issue_close` closes an existing issue.

- `issue_edit` edits the properties of an existing issue.

## Usage

``` r
issue_close(repo, number, comment = NULL)

issue_create(
  repo,
  title,
  body,
  labels = character(),
  assignees = character(),
  delay = 0
)

issue_edit(
  repo,
  number,
  title = NULL,
  body = NULL,
  state = NULL,
  milestone = NULL,
  labels = list(),
  assignees = list()
)
```

## Arguments

- repo:

  Character. Address of one or more repositories in `owner/name` format.

- number:

  Integer. GitHub issue number.

- comment:

  Character. Optional comment to post on the issue before closing it. If
  posting the comment fails the issue is not closed. Recycled against
  `repo` / `number`.

- title:

  Character. Title of the issue.

- body:

  Character. Content of the issue.

- labels:

  Character. Vector of the labels to associate with this issue

- assignees:

  Character. Vector of logins for users assigned to the issue.

- delay:

  Numeric. Delay between each API request. Issue creation has a
  secondary rate limit (~ 20/min).

- state:

  Character. State of the issue. Either "open" or "closed".

- milestone:

  Character. The number of the milestone to associate this issue with.
  Only users with push access can set the milestone for issues. The
  milestone is silently dropped otherwise.

## Value

All functions invisibly return a list containing the results of the
relevant GitHub API call.

## See also

[repo_issues](https://rundel.github.io/ghclass/reference/repo_details.md)

## Examples

``` r
if (FALSE) { # \dontrun{
repo_create("ghclass-test","test_issue")

issue_create(
  "ghclass-test/test_issue",
  title = "Issue 1",
  body = "This is an issue"
 )

issue_create(
  "ghclass-test/test_issue",
  title = "Issue 2", body = "This is also issue",
  label = "Important"
)

issue_create(
  "ghclass-test/test_issue",
  title = "Issue 3", body = "This is also issue",
  label = c("Important", "Super Important"),
  assignees = "rundel"
)

issue_close("ghclass-test/test_issue", 1)

issue_edit(
  "ghclass-test/test_issue", 2,
  title = "New issue 2 title!",
  body = "Replacement body text"
)

ghclass::repo_issues("ghclass-test/test_issue")

repo_delete("ghclass-test/test_issue", prompt=FALSE)
} # }
```
