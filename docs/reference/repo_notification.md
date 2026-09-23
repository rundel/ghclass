# GitHub Repository tools - notification functions

- `repo_ignore()` - Ignore a GitHub repository.

- `repo_unwatch()` - Unwatch / unsubscribe from a GitHub repository.

- `repo_watch()` - Watch / subscribe to a GitHub repository.

- `repo_watching()` - Returns a vector of your watched repositories.
  This should match the list at
  [github.com/watching](https://github.com/watching).

## Usage

``` r
repo_unwatch(repo)

repo_watch(repo)

repo_ignore(repo)

repo_watching(filter = NULL, exclude = FALSE)
```

## Arguments

- repo:

  repository address in `owner/repo` format

- filter:

  character, regex pattern for matching (or excluding) repositories.

- exclude:

  logical, should entries matching the regex be excluded or included.

## Value

`repo_ignore()`, `repo_unwatch()`, and `repo_watch()` all invisibly
return a list containing the results of the relevant GitHub API call.

`repo_watching()` returns a character vector of watched repos.

## Examples

``` r
if (FALSE) { # \dontrun{
repo_ignore("Sta323-Sp19/hw1")

repo_unwatch("rundel/ghclass")

repo_watch("rundel/ghclass")
} # }
```
