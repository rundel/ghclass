# GitHub user related tools

- `user_exists()` - returns `TRUE` if the username(s) (or organization)
  exist on GitHub and `FALSE` otherwise. Note that GitHub considers
  organizations to be a type of user.

- `user_repos()` - returns a (filtered) vector of repositories belonging
  to the user.

- `user_type()` - returns a vector of the accounts' types.

## Usage

``` r
user_exists(user)

user_repos(
  user,
  type = c("owner", "all", "public", "private", "member"),
  filter = NULL,
  exclude = FALSE,
  full_repo = TRUE
)

user_type(user)
```

## Arguments

- user:

  Character. GitHub username(s).

- type:

  Character. Can be one of "all", "owner", "public", "private",
  "member".

- filter:

  Character. Regular expression pattern for matching (or excluding)
  repositories.

- exclude:

  Logical. Should entries matching the regular expression in `filter` be
  excluded or included? Default `FALSE`.

- full_repo:

  Logical. Should the full repository address be returned (e.g.
  `owner/repo` instead of just `repo`)? Default `TRUE`.

## Value

`user_exists()` returns a logical vector.

`user_repos()` and `user_type()` retrun a character vector.

## Examples

``` r
if (FALSE) { # \dontrun{
user_exists(c("rundel", "ghclass-test", "hopefullydoesnotexist"))

user_repos("rundel", type = "public", filter = "ghclass")

user_repos("ghclass-test")

org_repos("ghclass-test")

user_type(c("rundel", "ghclass-test"))
} # }
```
