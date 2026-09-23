# Setup grading for an assignment

This is a higher level function that automates the common steps for
setting up grading of an assignment:

- Find repos matching a filter pattern in an organization

- Clone all matched repos locally

- Download GitHub Actions artifacts (e.g. rendered html, md files)

- Report any missing or out of sync artifacts

- Create comment template files for each repo

Artifacts are considered out of sync when they were built from a commit
other than the current commit of the cloned repo (e.g. the student
pushed after the last successful workflow run). These are skipped by
default, see `allow_stale`.

## Usage

``` r
org_grade_assignment(
  path,
  org,
  repo_filter,
  artifacts = character(),
  comment_template = "",
  key_repo = NULL,
  branch = NULL,
  allow_stale = FALSE
)
```

## Arguments

- path:

  Character. Root directory for the grading folder (created if it
  doesn't exist).

- org:

  Character. Name of the GitHub organization.

- repo_filter:

  Character. Regex pattern passed to
  [`org_repos()`](https://rundel.github.io/ghclass/reference/org_details.md)'s
  `filter` argument to select repos (e.g. `"hw01_"`).

- artifacts:

  Named character vector. Names are subfolder names, values are regex
  patterns passed as `filter` to
  [`action_artifacts()`](https://rundel.github.io/ghclass/reference/action.md)
  (matched against artifact names). E.g.
  `c("html" = "html-output", "report" = "pdf-report")`.

- comment_template:

  Character. Template text used to populate each comment markdown file.
  Defaults to `""`.

- key_repo:

  Character. Optional repository address in `owner/name` format to clone
  into the root of the grading folder as an answer key.

- branch:

  Character. Optional branch to clone and to collect artifacts from for
  the student repos. Defaults to each repo's default branch, in which
  case artifacts from any branch are considered. Does not apply to
  `key_repo`.

- allow_stale:

  Logical. Should out of sync artifacts be downloaded. Default `FALSE`,
  in which case they are reported and skipped.

## Value

An invisible list containing:

- `repos` — character vector of matched repo addresses

- `cloned` — result from
  [`local_repo_clone()`](https://rundel.github.io/ghclass/reference/local_repo.md)

- `artifacts` — named list of results from each
  [`action_artifact_download()`](https://rundel.github.io/ghclass/reference/action.md)
  call

- `stale` — named list of tibbles of out of sync artifacts for each
  entry of `artifacts`

- `comments` — character vector of comment file paths created

- `key` — result from cloning the key repo (if provided)
