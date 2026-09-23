# Tools for handling GitHub personal access tokens (PAT)

- `github_get_token` - returns the user's GitHub personal access token
  (PAT).

- `github_set_token` - defines the user's GitHub PAT by setting the
  `GITHUB_PAT` environmental variable. This value will persist until the
  session ends or `gihub_reset_token()` is called.

- `github_reset_token` - removes the value stored in the `GITHUB_PAT`
  environmental variable.

- `github_test_token` - checks if a PAT is valid by attempting to
  authenticate with the GitHub API.

- `github_token_scopes` - returns a vector of scopes granted to the
  token.

- `github_token_sitrep` - reports on the token: its type and source, the
  authenticated user, the granted scopes (flagging any that ghclass
  needs but are missing), and the API rate limit.

## Usage

``` r
github_rate_limit()

github_graphql_rate_limit()

github_get_token()

github_set_token(token)

github_reset_token()

github_test_token(token = github_get_token())

github_token_scopes(token = github_get_token())

github_token_sitrep(token = github_get_token())
```

## Arguments

- token:

  Character. Either the literal token, or the path to a file containing
  the token.

## Value

`github_get_token()` returns the current PAT as a character string with
the `gh_pat` class. See
[`gh::gh_token()`](https://gh.r-lib.org/reference/gh_token.html) for
additional details.

`github_set_token()` and `github_reset_token()` return the result of
[`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html) and
[`Sys.unsetenv()`](https://rdrr.io/r/base/Sys.setenv.html) respectively.

`github_test_token()` invisibly returns a logical value, `TRUE` if the
test passes, `FALSE` if not.

`github_token_scopes()` returns a character vector of granted scopes.

`github_token_sitrep()` invisibly returns a list with the token's type,
source, API url, the authenticated user's login, the granted scopes
(`NULL` when the token does not report them), the scopes ghclass uses
that are missing, and rate limit details.

## Details

This package looks for the personal access token (PAT) in the following
places (in order):

- Value of `GITHUB_PAT` environmental variable.

- A credential returned by
  [`gh::gh_token()`](https://gh.r-lib.org/reference/gh_token.html), such
  as `GITHUB_TOKEN`, a token stored with `gitcreds`, or a Posit Connect
  viewer token.

For additional details on creating a GitHub PAT see the usethis vignette
on [Managing Git(Hub)
Credentials](https://usethis.r-lib.org/articles/articles/git-credentials.html).
For those who do not wish to read the entire article, the quick start
method is to use:

- [`usethis::create_github_token()`](https://usethis.r-lib.org/reference/github-token.html) -
  to create the token and then,

- [`gitcreds::gitcreds_set()`](https://gitcreds.r-lib.org/reference/gitcreds_get.html) -
  to securely cache the token.

### Scopes

A classic PAT needs the `repo` and `admin:org` scopes for ghclass to
manage an organization's repositories and teams, the `workflow` scope to
add or modify files under `.github/workflows/`, the `notifications`
scope to use
[`repo_watch()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
[`repo_ignore()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
and
[`repo_unwatch()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
and the `delete_repo` scope to use
[`repo_delete()`](https://rundel.github.io/ghclass/reference/repo_core.md).
Note that
[`usethis::create_github_token()`](https://usethis.r-lib.org/reference/github-token.html)
does not select `admin:org` by default. Fine-grained tokens do not
report scopes, so `github_token_sitrep()` cannot check them. GitHub does
not support fine-grained or GitHub App tokens for
[`repo_watch()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
[`repo_ignore()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
or
[`repo_unwatch()`](https://rundel.github.io/ghclass/reference/repo_notification.md).

### GitHub Enterprise

To use ghclass with a GitHub Enterprise Server instance, set the
`GITHUB_API_URL` environment variable to the instance's REST API
endpoint, e.g. `https://github.example.edu/api/v3`. This is the same
variable used by [`gh::gh()`](https://gh.r-lib.org/reference/gh.html)
and is honored by all ghclass functions, including those that construct
clone, push, badge, and GraphQL URLs directly. Tokens are looked up per
host, so `gitcreds::gitcreds_set("https://github.example.edu")` can
store an Enterprise token alongside one for github.com.

## Examples

``` r
if (FALSE) { # \dontrun{
github_test_token()

github_token_scopes()

github_token_sitrep()

(pat = github_get_token())

github_set_token("ghp_BadTokenBadTokenBadTokenBadTokenBadToken")
github_get_token()
github_test_token()

github_set_token(pat)
} # }
```
