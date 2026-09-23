# Package index

## GitHub

- [`github_get_api_limit()`](https://rundel.github.io/ghclass/reference/github_api_limit.md)
  [`github_set_api_limit()`](https://rundel.github.io/ghclass/reference/github_api_limit.md)
  [`github_get_max_wait()`](https://rundel.github.io/ghclass/reference/github_api_limit.md)
  [`github_set_max_wait()`](https://rundel.github.io/ghclass/reference/github_api_limit.md)
  [`github_get_max_rate()`](https://rundel.github.io/ghclass/reference/github_api_limit.md)
  [`github_set_max_rate()`](https://rundel.github.io/ghclass/reference/github_api_limit.md)
  : Tools for limiting gh's GitHub api requests.

- [`github_orgs()`](https://rundel.github.io/ghclass/reference/github_orgs.md)
  : Collect details on the authenticated user's GitHub organization
  memberships (based on the current PAT).

- [`github_rate_limit()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_graphql_rate_limit()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_get_token()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_set_token()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_reset_token()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_test_token()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_token_scopes()`](https://rundel.github.io/ghclass/reference/github_token.md)
  [`github_token_sitrep()`](https://rundel.github.io/ghclass/reference/github_token.md)
  : Tools for handling GitHub personal access tokens (PAT)

- [`github_whoami()`](https://rundel.github.io/ghclass/reference/github_whoami.md)
  : Returns the login of the authenticated user (based on the current
  PAT).

- [`with_pat()`](https://rundel.github.io/ghclass/reference/github_with_pat.md)
  [`local_pat()`](https://rundel.github.io/ghclass/reference/github_with_pat.md)
  :

  `withr`-like functions for temporary personal access token

## Local Repositories

- [`local_repo_add()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_branch()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_clone()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_commit()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_log()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_pull()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_push()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  : Functions for managing local git repositories
- [`local_repo_anonymize()`](https://rundel.github.io/ghclass/reference/local_repo_anonymize.md)
  **\[experimental\]** : Anonymize a local repo or grading project
- [`local_repo_rename()`](https://rundel.github.io/ghclass/reference/local_repo_rename.md)
  : Rename local directories using a vector of patterns and
  replacements.

## Organizations

- [`org_create_assignment()`](https://rundel.github.io/ghclass/reference/org_create_assignment.md)
  : Create a team or individual assignment
- [`org_exists()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_repo_forking()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_repo_search()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_repo_stats()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_repos()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_team_details()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_teams()`](https://rundel.github.io/ghclass/reference/org_details.md)
  [`org_user_repos()`](https://rundel.github.io/ghclass/reference/org_details.md)
  : Obtain details on an organization's repos and teams
- [`org_grade_assignment()`](https://rundel.github.io/ghclass/reference/org_grade_assignment.md)
  : Setup grading for an assignment
- [`org_admins()`](https://rundel.github.io/ghclass/reference/org_members.md)
  [`org_invite()`](https://rundel.github.io/ghclass/reference/org_members.md)
  [`org_members()`](https://rundel.github.io/ghclass/reference/org_members.md)
  [`org_pending()`](https://rundel.github.io/ghclass/reference/org_members.md)
  [`org_remove()`](https://rundel.github.io/ghclass/reference/org_members.md)
  : Tools for managing organization membership
- [`org_allows_forking()`](https://rundel.github.io/ghclass/reference/org_perm.md)
  [`org_sitrep()`](https://rundel.github.io/ghclass/reference/org_perm.md)
  [`org_set_repo_permission()`](https://rundel.github.io/ghclass/reference/org_perm.md)
  [`org_set_permissions()`](https://rundel.github.io/ghclass/reference/org_perm.md)
  [`org_workflow_permissions()`](https://rundel.github.io/ghclass/reference/org_perm.md)
  [`org_set_workflow_permissions()`](https://rundel.github.io/ghclass/reference/org_perm.md)
  : Organization permissions

## Repositories

- [`local_repo_add()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_branch()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_clone()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_commit()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_log()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_pull()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  [`local_repo_push()`](https://rundel.github.io/ghclass/reference/local_repo.md)
  : Functions for managing local git repositories
- [`repo_allows_forking()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_create()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_delete()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_exists()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_is_template()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_mirror()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_mirror_template()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_rename()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_set_forking()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  [`repo_set_template()`](https://rundel.github.io/ghclass/reference/repo_core.md)
  : GitHub Repository tools - core functions
- [`repo_branches()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  [`repo_clone_url()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  [`repo_commits()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  [`repo_issues()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  [`repo_n_commits()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  [`repo_prs()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  [`repo_pushes()`](https://rundel.github.io/ghclass/reference/repo_details.md)
  : GitHub Repository tools - repository details
- [`repo_add_file()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_delete_file()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_get_file()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_get_readme()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_ls()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_modify_file()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_put_file()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  [`repo_tree()`](https://rundel.github.io/ghclass/reference/repo_file.md)
  : GitHub Repository tools - file functions
- [`repo_unwatch()`](https://rundel.github.io/ghclass/reference/repo_notification.md)
  [`repo_watch()`](https://rundel.github.io/ghclass/reference/repo_notification.md)
  [`repo_ignore()`](https://rundel.github.io/ghclass/reference/repo_notification.md)
  [`repo_watching()`](https://rundel.github.io/ghclass/reference/repo_notification.md)
  : GitHub Repository tools - notification functions
- [`repo_style()`](https://rundel.github.io/ghclass/reference/repo_style.md)
  : Style repository with styler
- [`repo_add_team()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_team_permission()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_add_user()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_user_permission()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_collaborators()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_contributors()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_remove_team()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  [`repo_remove_user()`](https://rundel.github.io/ghclass/reference/repo_user.md)
  : GitHub Repository tools - user functions

## Users

- [`user_exists()`](https://rundel.github.io/ghclass/reference/user.md)
  [`user_repos()`](https://rundel.github.io/ghclass/reference/user.md)
  [`user_type()`](https://rundel.github.io/ghclass/reference/user.md) :
  GitHub user related tools

## Teams

- [`team_create()`](https://rundel.github.io/ghclass/reference/team.md)
  [`team_delete()`](https://rundel.github.io/ghclass/reference/team.md)
  [`team_rename()`](https://rundel.github.io/ghclass/reference/team.md)
  : Create, delete, and rename teams within an organization
- [`team_invite()`](https://rundel.github.io/ghclass/reference/team_members.md)
  [`team_members()`](https://rundel.github.io/ghclass/reference/team_members.md)
  [`team_pending()`](https://rundel.github.io/ghclass/reference/team_members.md)
  [`team_remove()`](https://rundel.github.io/ghclass/reference/team_members.md)
  [`team_repos()`](https://rundel.github.io/ghclass/reference/team_members.md)
  : Tools for inviting, removing, and managing members of an
  organization team
- [`team_roster()`](https://rundel.github.io/ghclass/reference/team_roster.md)
  : Add team assignments to a roster

## Issues

- [`issue_close()`](https://rundel.github.io/ghclass/reference/issue.md)
  [`issue_create()`](https://rundel.github.io/ghclass/reference/issue.md)
  [`issue_edit()`](https://rundel.github.io/ghclass/reference/issue.md)
  : GitHub Issue related tools

## Branches

- [`branch_create()`](https://rundel.github.io/ghclass/reference/branch.md)
  [`branch_delete()`](https://rundel.github.io/ghclass/reference/branch.md)
  [`branch_remove()`](https://rundel.github.io/ghclass/reference/branch.md)
  : Create and delete branches in a repository

## Pull Requests

- [`pr_create()`](https://rundel.github.io/ghclass/reference/pr.md) :
  GitHub Pull Request related tools

## Actions

- [`action_artifacts()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_artifact_delete()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_artifact_download()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_run_logs()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_runs()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_status()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_runtime()`](https://rundel.github.io/ghclass/reference/action.md)
  [`action_workflows()`](https://rundel.github.io/ghclass/reference/action.md)
  : Retrieve information about GitHub Actions workflows and their runs.
- [`action_add_badge()`](https://rundel.github.io/ghclass/reference/action_badge.md)
  [`action_remove_badge()`](https://rundel.github.io/ghclass/reference/action_badge.md)
  : Add or remove GitHub Actions badges from a repository

## Pages

- [`pages_enabled()`](https://rundel.github.io/ghclass/reference/pages.md)
  [`pages_status()`](https://rundel.github.io/ghclass/reference/pages.md)
  [`pages_create()`](https://rundel.github.io/ghclass/reference/pages.md)
  [`pages_delete()`](https://rundel.github.io/ghclass/reference/pages.md)
  : Retrieve information about GitHub Pages sites and builds.
