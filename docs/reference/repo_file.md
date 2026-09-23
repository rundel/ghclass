# GitHub Repository tools - file functions

- `repo_add_file()` - Add / update files in a GitHub repository. Note
  that due to delays in caching, files that have been added very
  recently might not yet be displayed as existing and might accidentally
  be overwritten.

- `repo_delete_file()` - Delete a file from a GitHub repository

- `repo_modify_file()` - Modify an existing file within a GitHub
  repository.

- `repo_ls()` - Low level function for listing the files in a GitHub
  Repository

- `repo_tree()` - Print the file tree of a GitHub repository, similar to
  [`fs::dir_tree()`](https://fs.r-lib.org/reference/dir_tree.html)

- `repo_put_file()` - Low level function for adding a file to a GitHub
  repository

- `repo_get_file()` - Low level function for retrieving the content of a
  file from a GitHub Repository

- `repo_get_readme()` - Low level function for retrieving the content of
  the `README.md` of a GitHub Repository

## Usage

``` r
repo_add_file(
  repo,
  file,
  message = NULL,
  repo_folder = NULL,
  branch = NULL,
  preserve_path = FALSE,
  overwrite = FALSE
)

repo_delete_file(repo, path, message = NULL, branch = NULL)

repo_get_file(repo, path, branch = NULL, quiet = FALSE, include_details = TRUE)

repo_get_readme(repo, branch = NULL, include_details = TRUE)

repo_ls(repo, path = ".", branch = NULL, full_path = FALSE)

repo_modify_file(
  repo,
  path,
  pattern,
  content,
  method = c("replace", "before", "after"),
  all = FALSE,
  message = "Modified content",
  branch = NULL,
  verbose = TRUE
)

repo_put_file(
  repo,
  path,
  content,
  message = NULL,
  branch = NULL,
  verbose = TRUE
)

repo_tree(repo, path = ".", branch = NULL, recurse = TRUE)
```

## Arguments

- repo:

  Character. Address of repository in `owner/name` format.

- file:

  Character. Local file path(s) of file or files to be added.

- message:

  Character. Commit message.

- repo_folder:

  Character. Name of folder on repository to save the file(s) to. If the
  folder does not exist on the repository, it will be created.

- branch:

  Character. Name of branch to use.

- preserve_path:

  Logical. Should the local relative path be preserved. Default `FALSE`.

- overwrite:

  Logical. Should existing file or files with same name be overwritten.
  Default `FALSE`.

- path:

  Character. File's path within the repository.

- quiet:

  Logical. Should status messages be printed. Default `FALSE`.

- include_details:

  Logical. Should file details be attached as attributes. Default
  `TRUE`.

- full_path:

  Logical. Should the function return the full path of the files and
  directories. Default `FALSE`.

- pattern:

  Character. Regex pattern.

- content:

  Character or raw. Content of the file.

- method:

  Character. Should the content `replace` the matched pattern or be
  inserted `before` or `after` the match.

- all:

  Character. Should all instances of the pattern be modified (`TRUE`) or
  just the first (`FALSE`).

- verbose:

  Logical. Should success / failure messages be printed. Default `TRUE`.

- recurse:

  Logical. Should the listing recurse into subdirectories. Default
  `TRUE`.

## Value

`repo_delete_file()`, `repo_modify_file()`, and `repo_put_file()` all
invisibly return a list containing the results of the relevant GitHub
API calls.

`repo_ls()` returns a character vector of repo files in the given path.

`repo_tree()` invisibly returns a character vector of the displayed
paths.

`repo_get_file()` and `repo_get_readme()` return a character vector with
API results attached as attributes if `include_details = TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
repo = repo_create("ghclass-test", "repo_file_test", auto_init=TRUE)

repo_ls(repo, path = ".")

repo_tree(repo)

repo_get_readme(repo, include_details = FALSE)

repo_get_file(repo, ".gitignore", include_details = FALSE)

repo_modify_file(
  repo, path = "README.md", pattern = "repo_file_test",
  content = "\n\nHello world!\n", method = "after"
)

repo_get_readme(repo, include_details = FALSE)

repo_add_file(repo, file = system.file("DESCRIPTION", package="ghclass"))

repo_get_file(repo, "DESCRIPTION", include_details = FALSE)

repo_delete_file(repo, "DESCRIPTION")

repo_delete(repo, prompt=FALSE)
} # }
```
