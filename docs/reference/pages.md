# Retrieve information about GitHub Pages sites and builds.

- `pages_enabled()` - returns `TRUE` if a Pages site exists for the
  repo.

- `pages_status()` - returns more detailed information about a repo's
  Pages site.

- `pages_create()` - creates a Pages site for the provided repos.

- `pages_delete()` - deletes the Pages site for the provided repos.

## Usage

``` r
pages_enabled(repo)

pages_status(repo)

pages_create(
  repo,
  build_type = c("legacy", "workflow"),
  branch = "main",
  path = "/"
)

pages_delete(repo, prompt = TRUE)
```

## Arguments

- repo:

  Character. Address of repositories in `owner/name` format.

- build_type:

  Character. Either `"workflow"` or `"legacy"` - the former uses GitHub
  actions to build and publish the site (requires a workflow file to
  achieve this).

- branch:

  Character. Repository branch to publish.

- path:

  Character. Repository path to publish.

- prompt:

  Logical. Should the user be prompted before deleting Pages sites.
  Default `TRUE`.

## Value

`pages_enabled()` returns a named logical vector - `TRUE` if a Pages
site exists, `FALSE` otherwise.

`pages_status()` returns a tibble containing details on Pages sites.

`pages_create()` & `pages_delete()` return an invisible list containing
the API responses.

## Examples

``` r
if (FALSE) { # \dontrun{
pages_enabled("rundel/ghclass")

pages_status("rundel/ghclass")
} # }
```
