# ghclass 0.4.1

* Added `team_roster()` to assign roster members to teams of a given size, optionally within groups.

* Added experimental `local_repo_anonymize()` to scrub roster-derived student PII from the files of a local repo or grading project and delete (or keep) their git history.

* Fixed bugs in `repo_prs()`, `action_runs()`, and `action_runtime()` that produced wrong or missing values.

* Fixed several bugs in the `local_repo_*()` functions.

* GraphQL-based functions now escape special characters in names and filters and report API errors instead of failing silently.

* Improved argument validation and error reporting.

* `team_invite()` now de-duplicates repeated user / team pairs before inviting.

* Various fixes to the `action_*()` functions and file downloads.

* `action_artifact_download()` now treats unzip warnings as failures and removes partially extracted files (and the downloaded zip, unless `keep_zip`).

* `org_repo_search()` and `org_repo_stats()` now warn when a search exceeds GitHub's 1000-result limit.

* `team_members()`, `team_pending()`, and `team_repos()` gain a `slug` column; the `team` column now echoes the supplied team values.

* Added `github_set_max_wait()` and `github_set_max_rate()` (and getters) to control how long requests wait when rate limited and to throttle the request rate.

* GraphQL (v4) requests now retry with backoff when rate limited instead of failing immediately.

# ghclass 0.4.0

* Added `ignore_existing` argument to `org_create_assignment()` (#134, idea by @davidbuch).

* Added `committer_date` column to `repo_commits()` output (#139).

* Added `comment` argument to `issue_close()` (#136).

* `repo_contributors()` now retries on HTTP 202 and warns instead of silently returning empty (#117).

* `org_repo_stats()` now deduplicates rows by repo (#116).

* Added `org_user_repos()` (#103).

* Logical `@param` descriptions now state defaults (#39).

* Improvements to `action_artifacts()` and `action_artifact_download()`

  * Added `branch` column to `action_artifacts()` output

  * Added `filter_branch` / `exclude_branch` and `filter` / `exclude` arguments to both functions for filtering by branch name and artifact name respectively

  * `action_artifact_download()` better handles repos with multiple objects (errors only show for no matching artifacts)

  * `action_artifact_download()` disambiguates output filenames for repos with multiple artifacts by appending the artifact name.

  * Breaking: `action_artifact_download()` reworked. Removed `file_pat`; added `nest`. Each artifact is extracted into its own folder under `dir`; `keep_zip` controls whether the zip is retained. `org_grade_assignment()`'s `artifacts` values are now artifact-name filter patterns.

* Added `org_grade_assignment()` to automate grading setup: clones repos, downloads GitHub Actions artifacts, and creates comment template files.

* Added `repo_allows_forking()` to check if forking is enabled for a repository.

* Added `org_allows_forking()` to check if members can fork private repositories in an organization.

* Added `org_repo_forking()` to retrieve the forking status of all private repositories in an organization.

* Added `repo_set_forking()` to enable or disable forking for a repository.

* Tweaked how teams slugs are looked up due to API delays / caching issues which was causing errors in `org_create_assignment()`.

* Fixed a minor issue with data handling in `action_runs()`

# ghclass 0.3.1

* Added check to `repo_mirror_template()` for empty repositories to avoid cryptic GitHub api error.

* Added `repo_pushes()` to retrieve push activity for a repository.

* Added `org_workflow_permissions()` and `org_set_workflow_permissions()` to handle organization workflow permissions.

* Added workflow permissions details to `org_sitrep()`

* Bug fixes
  
  * Handle artifacts with large id values (causing an integer overflow)
  
  * Duplicate action badges from `org_create_assignment()`

* Altered `org_create_assignment()` to require the use of a template repository as `source_repo`.

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
