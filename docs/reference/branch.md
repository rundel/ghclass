# Create and delete branches in a repository

- `branch_create()` - creates a new branch from an existing GitHub repo.

- `branch_delete()` - deletes a branch from an existing GitHub repo.

- `branch_remove()` - previous name of `branch_delete`, deprecated.

## Usage

``` r
branch_create(repo, branch, new_branch)

branch_delete(repo, branch)

branch_remove(repo, branch)
```

## Arguments

- repo:

  GitHub repository address in `owner/repo` format.

- branch:

  Repository branch to use.

- new_branch:

  Name of branch to create.

## Value

`branch_create()` and `branch_delete()` invisibly return a list
containing the results of the relevant GitHub API call.

## See also

[repo_branches](https://rundel.github.io/ghclass/reference/repo_details.md)

## Examples

``` r
if (FALSE) { # \dontrun{
repo_create("ghclass-test", "test_branch", auto_init=TRUE)

branch_create("ghclass-test/test_branch", branch = "main", new_branch = "test")
repo_branches("ghclass-test/test_branch")

branch_delete("ghclass-test/test_branch", branch="test")
repo_branches("ghclass-test/test_branch")

repo_delete("ghclass-test/test_branch", prompt = FALSE)
} # }
```
