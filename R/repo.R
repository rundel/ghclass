#' @name repo_core
#' @rdname repo_core
#'
#' @title GitHub Repository tools - core functions
#'
#' @description
#'
#' * `repo_create` - create a GitHub repository.
#'
#' * `repo_delete` - delete a GitHub repository.
#'
#' * `repo_rename` - rename a repository, note that renamed repositories retain their
#' unique identifier and can still be accessed via their old names due to
#' GitHub re-directing.
#'
#' * `repo_exists` - returns `TRUE` if the GitHub repository exists. It will also print
#'  a message if a repository has been renamed, unless `quiet = TRUE`.
#'
#' * `repo_mirror` mirror the content of a repository to another repository,
#' the target repo must already exist.
#'
#' * `repo_mirror_template` - mirror the content of a source template repository to a new repository,
#' the target repo must *not* already exist.
#'
#' * `repo_is_template` - returns `TRUE` if a repository is a template repo.
#'
#' * `repo_set_template` - change the template status of a repository.
#'
#' @param repo Character. Address of repository in `owner/repo` format.
#'
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test", "repo_test")
#'
#' repo_exists("ghclass-test/repo_test")
#'
#' repo_rename("ghclass-test/repo_test", "repo_test_new")
#'
#'
#' # The new repo exists
#' repo_exists("ghclass-test/repo_test_new")
#'
#' # The old repo forwards to the new repo
#' repo_exists("ghclass-test/repo_test")
#'
#'
#' # Check for the redirect by setting `strict = TRUE`
#' repo_exists("ghclass-test/repo_test", strict = TRUE)
#'
#'
#' # The prefered way of copying a repo is by making the source a template
#' repo_is_template("ghclass-test/repo_test_new")
#'
#' repo_set_template("ghclass-test/repo_test_new")
#'
#' repo_is_template("ghclass-test/repo_test_new")
#'
#'
#' # Given a template repo we can then directly copy the repo on GitHub
#' repo_mirror_template("ghclass-test/repo_test_new", "ghclass-test/repo_test_copy")
#'
#' repo_exists("ghclass-test/repo_test_copy")
#'
#' # Cleanup
#' repo_delete(
#'  c("ghclass-test/repo_test_new",
#'    "ghclass-test/repo_test_copy"),
#'  prompt = FALSE
#' )
#'
#' }
#'
#'
NULL


#' @name repo_details
#' @rdname repo_details
#'
#' @title GitHub Repository tools - repository details
#'
#' @description
#'
#' * `repo_clone_url` - Returns the url, for cloning, a GitHub repo (either ssh or https)
#'
#' * `repo_branches` - Returns a (filtered) vector of branch names.
#'
#' * `repo_commits` - Returns a tibble of commits to a GitHub repository.
#'
#' * `repo_issues` - Returns a tibble of issues for a GitHub repository.
#'
#' * `repo_n_commits` - Returns a tibble of the number of commits in a GitHub repository (and branch).
#'
#' * `repo_prs` - Returns a tibble of pull requests for a GitHub repository.
#'
#' @param repo Character. Address of repository in `owner/repo` format.
#'
#'
#' @examples
#' \dontrun{
#' repo_clone_url("rundel/ghclass")
#'
#' repo_branches("rundel/ghclass")
#'
#' repo_commits("rundel/ghclass")
#'
#' repo_issues("rundel/ghclass")
#'
#' repo_n_commits("rundel/ghclass")
#'
#' repo_prs("rundel/ghclass")
#' }
#'
NULL


#' @name repo_file
#' @rdname repo_file
#'
#' @title GitHub Repository tools - file functions
#'
#' @description
#'
#' * `repo_add_file` - Add / update files in a GitHub repository.
#' Note that due to time delays in caching, files that have been added
#' very recently might not yet be displayed as existing and might accidentally
#' be overwritten.
#'
#' * `repo_delete_file` - Delete a file from a Github repository
#'
#' * `repo_modify_file` - Modify an existing file within a GitHub repository.
#'
#' * `repo_ls` - Low level function for listing the files in a GitHub Repository
#'
#' * `repo_put_file` - Low level function for adding a file to a Github repository
#'
#' * `repo_get_file` - Low level function for retrieving the content of a file from a GitHub Repository
#'
#' * `repo_get_readme` - Low level function for retrieving the content of the `README.md` of a GitHub Repository
#'
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param path Character. File's path within the repository.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param quiet Logical. Should status messages be printed.
#' @param include_details Logical. Should file details be attached as attributes.
#'
#' @examples
#' \dontrun{
#' repo = repo_create("ghclass-test", "repo_file_test", auto_init=TRUE)
#'
#' repo_ls(repo, path = ".")
#'
#' repo_get_readme(repo, include_details = FALSE)
#'
#' repo_get_file(repo, ".gitignore", include_details = FALSE)
#'
#' repo_modify_file(
#'   repo, path = "README.md", pattern = "repo_file_test",
#'   content = "\n\nHello world!\n", method = "after"
#' )
#'
#' repo_get_readme(repo, include_details = FALSE)
#'
#' repo_add_file(repo, file = system.file("DESCRIPTION", package="ghclass"))
#'
#' repo_get_file(repo, "DESCRIPTION", include_details = FALSE)
#'
#' repo_delete_file(repo, "DESCRIPTION")
#'
#' repo_delete(repo, prompt=FALSE)
#' }
#'
NULL


#' @name repo_notification
#' @rdname repo_notification
#'
#' @title GitHub Repository tools - notification functions
#'
#' @description
#'
#' * `repo_ignore` - Ignore a GitHub repository.
#'
#' * `repo_unwatch` - Unwatch / unsubscribe from a GitHub repository.
#'
#' * `repo_watch` - Watch / subscribe to a GitHub repository.
#'
#' * `repo_watching` - Returns a vector of your watched repositories. This should
#' match the list at [github.com/watching](https://github.com/watching).
#'
#' @param repo repository address in `owner/repo` format
#'
#'
#' @examples
#' \dontrun{
#' repo_ignore("Sta323-Sp19/hw1")
#'
#' repo_unwatch("rundel/ghclass")
#'
#' repo_watch("rundel/ghclass")
#' }
#'
NULL


#' @name repo_user
#' @rdname repo_user
#'
#' @title GitHub Repository tools - user functions
#'
#' @description
#'
#' * `repo_add_user` - Add a user to a repository
#' * `repo_remove_user` - Remove a user from a repository
#' * `repo_add_team` - Add a team to a repository
#' * `repo_remove_team` - Remove a team from a repository
#' * `repo_collaborators` - Returns a data frame of repos, their collaborators, and their permissions.
#' * `repo_contributors` - Returns a data frame containing details on repository contributor(s).
#'
#' @param repo Character. Address of repository in `owner/repo` format.
#' @param user Character. One or more GitHub usernames.
#' @param permission Character. Permission to be granted to a user or team for repo, defaults to "push".
#' @param team Character. Slug or name of team to add.
#' @param team_type Character. Either "slug" if the team names are slugs or "name" if full team names are provided.
#' @param include_admins Logical. If `FALSE`, user names of users with Admin rights are not included, defaults to `TRUE`.
#'
#' @details
#'
#' Permissions can be set to any of the following:
#' * `"pull"` - can pull, but not push to or administer this repository.
#' * `"push"` - can pull and push, but not administer this repository.
#' * `"admin"` - can pull, push and administer this repository.
#' * `"maintain"` - Recommended for project managers who need to manage the repository without access to sensitive or destructive actions.
#' * `"triage"` - Recommended for contributors who need to proactively manage issues and pull requests without write access.
#'
#'
#' @examples
#' \dontrun{
#' repo = repo_create("ghclass-test", "hw1")
#'
#' team_create("ghclass-test", "team_awesome")
#'
#' repo_add_user(repo, "rundel")
#'
#' repo_add_team(repo, "team_awesome")
#'
#' repo_remove_team(repo, "team_awesome")
#'
#' repo_collaborators(repo)
#'
#' repo_contributors(repo)
#' repo_contributors("rundel/ghclass")
#'
#' # Cleanup
#' repo_delete(repo, prompt=FALSE)
#' }
#'
NULL
