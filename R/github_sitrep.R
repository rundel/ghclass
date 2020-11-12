
#' Reports on the current GitHub configuration and access privledges. See more details on scopes
#' [here](https://developer.github.com/apps/building-oauth-apps/understanding-scopes-for-oauth-apps/).
#'
#' @examples
#' \dontrun{
#' github_sitrep()
#' }
#'
#' @export
#'
github_sitrep = function() {
  login = github_whoami()
  scopes = github_token_scopes()

  if (!github_test_token(quiet = TRUE)) {
    ghclass:::cli_stop("Failed to authenticate with GitHub (check your PAT is correct).")
  }


  cli::cli_h1("{.strong GitHub sitrep:}")
  cli::cli_ul()
  cli::cli_li("Authenticated as {.val {login}}")


  cli::cli_li("GitHub PAT privledges (OAuth scopes):")
  cli::cli_ul()

  cli::cli_li(cli_kv("Can fully manage organizations", "admin:org" %in% scopes))

  cli::cli_li(cli_kv("Can access both public and private repos", "repo" %in% scopes))
  if (!"repo" %in% scopes) {
    cli::cli_li(cli_kv("Can access public repos", "repo" %in% scopes))
    cli::cli_li(cli_kv("Can invite users to repos", "repo:invite" %in% scopes))
  }

  cli::cli_li(cli_kv("Can delete repositories", "delete_repo" %in% scopes))

  cli::cli_li(cli_kv("Can create GitHub Actions workflows", "workflow" %in% scopes))

  cli::cli_li(cli_kv("Can create gists", "gist" %in% scopes))

  cli::cli_end()

  cli::cli_end()
}
