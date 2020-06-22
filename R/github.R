#' @name github
#' @rdname github
#'
#' @title GitHub API related tools
#'
#' @description
#' `github_whoami` returns the login of the authenticated user.
#'
#' `github_get_api_limit` / `github_set_api_limit` - get and set the limit of results to return from
#' the GitHub API.
#'
#' `github_get_token` returns the user's GitHub personal access token (PAT).
#'
#' `github_set_token` defines the user's GitHub PAT by setting the `GITHUB_PAT` enivronmental variable. This value can then
#'
#' `github_reset_token` removes the value stored in the `GITHUB_PAT` environmental variable.
#'
#' `github_test_token` checks if a PAT is valid by attempting to authenticate with the GitHub API
#'
#' @details
#' This package looks for the personal access token (PAT) in the following places (in order):
#' * Value of `GITHUB_PAT` environmental variable.
#' * Value of `GITHUB_TOKEN` environmental variable.
#' * Contents of `~/.github/token` file.
#'
#' @seealso [repo_branches]
#'
#' @examples
#' # whoami
#' github_whoami()
#'
#' # Set the API request limit
#' github_get_api_limit()
#' github_set_api_limit(500)
#' github_get_api_limit()
#'
#' # Set and get the PAT
#' github_set_token("~/.github/token")
#' github_set_token("0123456789ABCDEF")
#' pat = github_get_token()
#' pat
#'
#' # Test the PAT
#' github_reset_token()
#' github_test_token()
#' github_test_token("bad_token")

NULL

