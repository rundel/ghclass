# Anonymize a local repo or grading project

**\[experimental\]**

Makes a best-effort pass over a local git repository, a directory of
repositories, or a grading project created by
[`org_grade_assignment()`](https://rundel.github.io/ghclass/reference/org_grade_assignment.md)
and removes student personally identifying information (PII). Two things
happen:

- **File contents** - every text file is scanned and any roster-derived
  value (names, NetIDs, emails, GitHub usernames, ...) is replaced with
  a stable per-student token (e.g. `student_07`).

- **Git history** - each repository's `.git` directory is either deleted
  outright (the default) or kept untouched.

## Usage

``` r
local_repo_anonymize(
  path,
  roster,
  cols = tidyselect::any_of(c("name", "first", "last", "email", "netid", "github")),
  output = NULL,
  types = c(".md", ".qmd", ".[Rr]md", ".txt", ".csv", ".tsv", ".html?", ".[Rr]", ".py",
    ".ipynb", ".json", ".ya?ml", ".tex", ".bib", ".toml", ".[Rr]proj"),
  git_history = c("delete", "keep"),
  prompt = interactive()
)
```

## Arguments

- path:

  Character. Path to a git repository, a directory of repositories, or a
  grading project folder (one containing a `repos/` subdirectory of
  cloned repositories, e.g. as produced by
  [`org_grade_assignment()`](https://rundel.github.io/ghclass/reference/org_grade_assignment.md)).

- roster:

  Either a path to a roster CSV file or a data frame. Each row is
  treated as one student.

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Roster columns whose values should be stripped from the contents.
  Defaults to the `name`, `first`, `last`, `email`, `netid`, and
  `github` columns when present.

- output:

  Character. Directory to write the anonymized copy to. Defaults to
  `<path>_anon`. Set `output = path` to modify the folder in place
  (gated by a confirmation prompt when `prompt = TRUE`).

- types:

  Character. Regular expressions matched against the end of each file
  name to decide which files are treated as text and scanned. Defaults
  to common text formats found in student work (markdown, R / Python
  source, notebooks, html, csv, yaml, ...).

- git_history:

  Character. How each repository's git history is handled. Either
  `"delete"` (default, remove the `.git` directory entirely) or `"keep"`
  (leave it untouched, retaining the un-anonymized history).

- prompt:

  Logical. Prompt for confirmation before modifying a folder in place.
  Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

Invisibly, a list with two tibbles: `text` (files scrubbed and
replacements made per directory) and `git` (the action taken per repo).

## Details

Git history cannot be reliably anonymized while also being preserved.
Even after rewriting commit author and committer identities, the
*original file contents* of older commits remain recoverable
(`git show <old-sha>:README.md`), and real names routinely survive in
commit messages and branch names. Because anyone you hand the repository
to receives its full history, there are only two honest options,
selected with `git_history`:

- `"delete"` (default) - remove each repository's `.git` directory
  entirely. The result is the scrubbed working tree with no recoverable
  history. This is the safe choice for sharing student work.

- `"keep"` - leave `.git` untouched. The working-tree files are still
  scrubbed, but the original commit history (with real names and emails)
  is retained, so the result is **not** safe to distribute.

The function works on a *copy* by default (`<path>_anon`), leaving the
original intact, so deleting history is non-destructive to your working
copy.

Only files whose names match `types` are scanned, so binary and other
non-text files are skipped by design. Matching is best effort: word
boundaries are used for names / NetIDs to avoid clobbering unrelated
text, but short or common names may still over-match, and
HTML-entity-encoded names are not detected. Anonymization is
roster-driven, so anyone absent from the roster (e.g. a student who
later dropped) will not be matched.

## Examples

``` r
if (FALSE) { # \dontrun{
local_repo_anonymize(
  "grading/hw1",
  roster = "rosters/hw1_roster.csv"
)

# scrub text but keep history, choosing the columns explicitly
local_repo_anonymize(
  "grading/hw1",
  roster = roster_df,
  cols = c(name, netid, email),
  git_history = "keep"
)
} # }
```
