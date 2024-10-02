# ghclass (development version)

* Added check to `repo_mirror_template()` for empty repositories to avoid cryptic GitHub api error.

* Added `repo_pushes()` to retrieve push activity for a repository.

# ghclass 0.3.0

* Added support for basig GitHub Pages API endpoints - see `pages_enabled()`, `pages_status()`, `pages_create()`, and `pages_delete()`.

* Added support for retrieving details for GitHub Action artifacts via `action_artifacts()`

* Added support for downloading GitHub Action artifacts via `action_artifact_download()`

* Added support for deleting GitHub Action artifacts via `action_artifact_delete()`

* Added additional parameters to `org_repos()` to control which repos are returns and in what order, see function docs.

* Reworked `action_runs()` to work with multiple repositories and included new filtering options. This makes `action_status()` redundant and as such it has been deprecated.

* Added `action_runtime()` which supplements the results of `action_runs()` with the duration for each action run.

* Added `github_token_scopes()` which returns a vector of granted scopes for the given PAT.

* Added `github_rate_limit()` & `github_graphql_rate_limit()` for checking current rate limit status.

* Added `add_badges` argument to `org_create_assignment()`

* Added support for GitHub's versioned REST API

* Increased the default value of `github_set_api_limit()` to 10,000.

* Any rest API request will now report if the request limit is reached via a warning.

# ghclass 0.2.1

* ghclass is now on [CRAN](https://CRAN.R-project.org/package=ghclass)!

* improved action support with `action_runs()` and `action_status()`

* revised how GitHub PATs are handled - now uses `gitcreds` by way of ` gh::gh_token()`

* users are now warned about PATs in `.Renviron` a-la `usethis:::scold_for_renviron()`

* Added version dependency on `cli` 3.0.0

* Added support for default branches with `org_repo_stats()`

# ghclass 0.2.0

* First official release of ghclass

* Switched underlying ui tools to use cli instead of usethis

* Major cleanup and rewrite of many functions

* Basic support for GitHub actions added

* `peer_*` functions are moved to the peer_review branch for now, coming back in 0.2.1. See README for details if you need them before the next release.

* The use of "master" as the default value for branch arguments has been removed across the package. Most functions with a branch argument will continue to function in the same way and will use whatever is defined as the default branch for that repository. In a small handful of cases where the API does not allow this, the argument will now require the user to provide an explicit value. These changes were made in anticipation of GitHub moving the default branch from "master" to "main" in October (see https://github.com/github/renaming for details).
