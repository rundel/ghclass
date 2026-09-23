# Style repository with styler

- `repo_style` implements "non-invasive pretty-printing of R source
  code" of .R or .Rmd files within a repository using the `styler`
  package and adhering to `tidyverse` formatting guidelines.

## Usage

``` r
repo_style(
  repo,
  files = c("*.R", "*.Rmd"),
  branch = "styler",
  base,
  create_pull_request = TRUE,
  draft = TRUE,
  tag_collaborators = TRUE,
  prompt = TRUE
)
```

## Arguments

- repo:

  Character. Address of repository in "owner/name" format.

- files:

  Character or vector of characters. Names of .R and/or .Rmd files that
  styler should be applied to.

- branch:

  Character. Name of new branch to be created or overwritten. Default is
  "styler".

- base:

  Character. Name of branch that contains the .R and/or .Rmd files to be
  styled

- create_pull_request:

  Logical. If TRUE, a pull request is created from branch to base.
  Default `TRUE`.

- draft:

  Logical. Should the pull request be created as a draft pull request?
  (Draft PRs cannot be merged until allowed by the author) Default
  `TRUE`.

- tag_collaborators:

  Logical. If TRUE, a message with the repository collaborators is
  displayed. Default `TRUE`.

- prompt:

  Logical. Prompt the user before overwriting an existing branch.
  Default `TRUE`.

## Value

The functions returns NULL invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
repo_style("ghclass-test/hw1-team01", base = "main")
} # }
```
