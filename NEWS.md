# ghclass (development version)

# ghclass 0.2.1.9000

* Added support for retrieving details for GitHub Action artifacts via `action_artifacts()`

* Added support for downloading GitHub Action artifacts via `action_artifact_download()`

* Added additional parameters to `org_repos()` to control which repos are returns and in what order, see function docs.

# ghclass 0.2.1

* ghclass is now on [CRAN](https://cran.r-project.org/web/packages/ghclass/)!

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
