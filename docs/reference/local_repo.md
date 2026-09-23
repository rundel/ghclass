# Functions for managing local git repositories

- `local_repo_clone()` - Clones a GitHub repository to a local
  directory.

- `local_repo_add()` - Equivalent to `git add` - stages a file in a
  local repository.

- `local_repo_commit()` - Equivalent to `git commit` - commits staged
  files in a local repository.

- `local_repo_push()` - Equivalent to `git push` - push a local
  repository.

- `local_repo_pull()` - Equivalent to `git pull` - pull a local
  repository.

- `local_repo_branch()` - Equivalent to `git branch` - create a branch
  in a local repository.

- `local_repo_log()` - Equivalent to `git log` - returns a data frame
  for git log entries.

## Usage

``` r
local_repo_add(repo_dir, files = ".")

local_repo_branch(repo_dir, branch)

local_repo_clone(
  repo,
  local_path = ".",
  branch = NULL,
  mirror = FALSE,
  verbose = FALSE
)

local_repo_commit(repo_dir, message)

local_repo_log(repo_dir, max = 100)

local_repo_pull(repo_dir, verbose = FALSE)

local_repo_push(
  repo_dir,
  remote = "origin",
  branch = NULL,
  force = FALSE,
  prompt = TRUE,
  mirror = FALSE,
  verbose = FALSE
)
```

## Arguments

- repo_dir:

  Vector of repo directories or a single directory containing one or
  more repos.

- files:

  Files to be staged

- branch:

  Repository branch to use.

- repo:

  GitHub repo address with the form `owner/name`.

- local_path:

  Local directory to store cloned repos.

- mirror:

  Equivalent to `--mirror`

- verbose:

  Display verbose output.

- message:

  Commit message

- max:

  Maximum number of log entries to retrieve per repo.

- remote:

  Repository remote to use.

- force:

  Force push?

- prompt:

  Prompt before force push?

## Value

`local_repo_clone()` invisibly returns a character vector of paths for
the local repo directories.

`local_repo_log()` returns a tibble containing repository details.\`

All other functions invisibly return a list containing the results of
the relevant call to `gert`.

## Details

All `local_repo_*` functions depend on the `gert` library being
installed.

## Examples

``` r
if (FALSE) { # \dontrun{
repo = repo_create("ghclass-test", "local_repo_test")

dir = file.path(tempdir(), "repos")
local_repo = local_repo_clone(repo, dir)

local_repo_log(dir)

# Make a local change and push
writeLines("Hello World", file.path(local_repo, "hello.txt"))

local_repo_add(local_repo, "hello.txt")

local_repo_commit(local_repo, "Added hello world")

local_repo_push(local_repo)

repo_commits(repo)

# Pulling remote changes
repo_modify_file(repo, "hello.txt", pattern = ".*",  content = "!!!", method = "after")

local_repo_pull(local_repo)

local_repo_log(dir)

repo_delete("ghclass-test/local_repo_test", prompt=FALSE)
} # }
```
