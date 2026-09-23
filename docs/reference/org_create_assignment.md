# Create a team or individual assignment

This is a higher level function that combines the following steps:

- Create the assignment repos, generated from `source_repo` if provided
  (which must be a template repository, see
  [`repo_set_template()`](https://rundel.github.io/ghclass/reference/repo_core.md))

- Create the teams and add the students to them (team assignments only)

- Give each team, or each individual student, push access to their repo

- Optionally add GitHub Actions status badges to each README
  (`add_badges = TRUE`)

## Usage

``` r
org_create_assignment(
  org,
  repo,
  user,
  team = NULL,
  source_repo = NULL,
  private = TRUE,
  add_badges = FALSE,
  ignore_existing = FALSE
)
```

## Arguments

- org:

  Character. Name of the GitHub organization.

- repo:

  Character. Name of the repo(s) for the assignment.

- user:

  Character. GitHub username(s).

- team:

  Character. Team names, if not provided an individual assignment will
  be created.

- source_repo:

  Character. Address of the repository to use as a template for all
  created repos.

- private:

  Logical. Should the created repositories be private. Default `TRUE`.

- add_badges:

  Logical. Should GitHub action badges be added to the README. Default
  `FALSE`.

- ignore_existing:

  Logical. If `TRUE`, any requested repos that already exist are skipped
  (with a warning) and the remaining repos are still created; useful for
  resuming a partially failed run. Default `FALSE`.

## Value

An invisible list containing the results of each step.
