# GitHub Pull Request related tools

- `pr_create()` - create a pull request GitHub from the `base` branch to
  the `head` branch.

## Usage

``` r
pr_create(repo, title, head, base, body = "", draft = FALSE)
```

## Arguments

- repo:

  Character. Address of one or more repositories in "owner/name" format.

- title:

  Character. Title of the pull request.

- head:

  Character. The name of the branch where your changes are implemented.
  For cross-repository pull requests in the same network, namespace
  `head` with a user like this: `username:branch`.

- base:

  Character. The name of the branch you want the changes pulled into.
  This should be an existing branch on the current repository. You
  cannot submit a pull request to one repository that requests a merge
  to a base of another repository.

- body:

  Character. The text contents of the pull request.

- draft:

  Logical. Should the pull request be created as a draft pull request
  (these cannot be merged until allowed by the author). Default `FALSE`.

## Value

`pr_create()` invisibly return a list containing the results of the
relevant GitHub API calls.

## See also

[repo_issues](https://rundel.github.io/ghclass/reference/repo_details.md)

## Examples

``` r
if (FALSE) { # \dontrun{
repo_create("ghclass-test", "test_pr", auto_init=TRUE)

branch_create("ghclass-test/test_pr", branch = "main", new_branch = "test")

repo_modify_file("ghclass-test/test_pr", "README.md", pattern = "test_pr",
                 content = "Hello", method = "after", branch = "test")

pr_create("ghclass-test/test_pr", title = "merge", head = "test", base = "main")

repo_delete("ghclass-test/test_pr", prompt = FALSE)
} # }
```
