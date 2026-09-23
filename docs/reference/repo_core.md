# GitHub Repository tools - core functions

- `repo_create()` - create a GitHub repository.

- `repo_delete()` - delete a GitHub repository.

- `repo_rename()` - rename a repository, note that renamed repositories
  retain their unique identifier and can still be accessed via their old
  names due to GitHub re-directing.

- `repo_exists()` - returns `TRUE` if the GitHub repository exists. It
  will also print a message if a repository has been renamed, unless
  `quiet = TRUE`.

- `repo_mirror()` - mirror the content of a repository to another
  repository, the target repo must already exist.

- `repo_mirror_template()` - mirror the content of a source template
  repository to a new repository, the target repo must *not* already
  exist.

- `repo_is_template()` - returns `TRUE` if a repository is a template
  repo.

- `repo_set_template()` - change the template status of a repository.

- `repo_allows_forking()` - returns `TRUE` if forking is enabled for a
  repository.

- `repo_set_forking()` - enable or disable forking for a repository.

## Usage

``` r
repo_allows_forking(repo)

repo_create(
  org,
  name,
  prefix = "",
  suffix = "",
  private = TRUE,
  auto_init = FALSE,
  gitignore_template = "R"
)

repo_delete(repo, prompt = TRUE)

repo_exists(repo, strict = FALSE, quiet = FALSE)

repo_is_template(repo)

repo_mirror(
  source_repo,
  target_repo,
  overwrite = FALSE,
  verbose = FALSE,
  warn = TRUE
)

repo_mirror_template(source_repo, target_repo, private = TRUE)

repo_rename(repo, new_repo)

repo_set_forking(repo, status = TRUE)

repo_set_template(repo, status = TRUE)
```

## Arguments

- repo:

  Character. Address of repository in `owner/repo` format.

- org:

  Character. GitHub organization that will own the repository

- name:

  Character. Repository name

- prefix:

  Character. Common repository name prefix

- suffix:

  Character. Common repository name suffix

- private:

  Logical. Should the new repository be private or public. Default
  `TRUE`.

- auto_init:

  Logical. Should the repository be initialized with a `README.md`.
  Default `FALSE`.

- gitignore_template:

  Character. `.gitignore` language template to use.

- prompt:

  Logical. Should the user be prompted before deleting repositories.
  Default `true`.

- strict:

  Logical. Should the old name of a renamed repositories be allowed.
  Default `FALSE`.

- quiet:

  Logical. Should details on renamed repositories be printed. Default
  `FALSE`.

- source_repo:

  Character. Address of template repository in `owner/name` format.

- target_repo:

  Character. One or more repository addresses in `owner/name` format.
  Note when using template repos these new repositories must *not*
  exist.

- overwrite:

  Logical. Should the target repositories be overwritten. Default
  `FALSE`.

- verbose:

  Logical. Display verbose output. Default `FALSE`.

- warn:

  Logical. Warn the user about the function being deprecated. Default
  `TRUE`.

- new_repo:

  Character. New name of repository without the owner.

- status:

  Logical. Should the repository be set as a template repository.
  Default `TRUE`.

## Value

`repo_create()` returns a character vector of created repos (in
`owner/repo` format)

`repo_exists()`, `repo_is_template()`, and `repo_allows_forking()` all
return a logical vector.

`repo_set_forking()` invisibly returns a list containing the results of
the relevant GitHub API calls.

All other functions invisibly return a list containing the results of
the relevant GitHub API calls.

## Examples

``` r
if (FALSE) { # \dontrun{
repo_create("ghclass-test", "repo_test")

repo_exists("ghclass-test/repo_test")

repo_rename("ghclass-test/repo_test", "repo_test_new")


# The new repo exists
repo_exists("ghclass-test/repo_test_new")

# The old repo forwards to the new repo
repo_exists("ghclass-test/repo_test")


# Check for the redirect by setting `strict = TRUE`
repo_exists("ghclass-test/repo_test", strict = TRUE)


# The prefered way of copying a repo is by making the source a template
repo_is_template("ghclass-test/repo_test_new")

repo_set_template("ghclass-test/repo_test_new")

repo_is_template("ghclass-test/repo_test_new")


# Given a template repo we can then directly copy the repo on GitHub
repo_mirror_template("ghclass-test/repo_test_new", "ghclass-test/repo_test_copy")

repo_exists("ghclass-test/repo_test_copy")

# Cleanup
repo_delete(
 c("ghclass-test/repo_test_new",
   "ghclass-test/repo_test_copy"),
 prompt = FALSE
)

} # }

```
