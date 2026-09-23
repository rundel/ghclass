# Rename local directories using a vector of patterns and replacements.

This function is meant to help with renaming local student repos to
include something more useful like `Last, First name` or a unique
identifier for the purposes of ordering repository folders.

## Usage

``` r
local_repo_rename(repo_dir, pattern, replacement)
```

## Arguments

- repo_dir:

  Character. A single directory containing one or more repos.

- pattern:

  Character. One or more regexp patterns to match to directory names.

- replacement:

  Character. One or more text strings containing the replacement value
  for matched patterns.

## Value

Returns a character vector of the new repo directory paths, or `NA` if
the rename failed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Prefix each cloned repo directory with a roster identifier
local_repo_rename(
  "/path/to/cloned/repos",
  pattern = "hw1-(.*)",
  replacement = "\\1"
)
} # }
```
