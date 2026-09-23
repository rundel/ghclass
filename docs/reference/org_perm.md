# Organization permissions

- `org_sitrep()` - Provides a situation report on a GitHub organization.

- `org_set_repo_permission()` - Change the default permission level for
  org repositories.

- `org_set_permissions()` - Change organization member privileges: the
  default repository permission and whether members can create
  repositories, fork private repositories, or create teams. Settings
  left as `NULL` are not changed, and any setting GitHub does not apply
  is reported with a warning. The remaining member privileges reported
  by `org_sitrep()` (visibility changes, repository and issue deletion,
  outside collaborators) are read-only in the GitHub API.

- `org_workflow_permissions()` - Obtain the current default workflow
  permission value for the organization.

- `org_set_workflow_permissions()` - Change the current default workflow
  permission value for the organization.

- `org_allows_forking()` - returns `TRUE` if members can fork private
  repositories in the organization.

## Usage

``` r
org_allows_forking(org)

org_sitrep(org)

org_set_repo_permission(
  org,
  repo_permission = c("none", "read", "write", "admin")
)

org_set_permissions(
  org,
  repo_permission = NULL,
  create_repositories = NULL,
  create_public_repositories = NULL,
  create_private_repositories = NULL,
  fork_private_repositories = NULL,
  create_teams = NULL
)

org_workflow_permissions(org)

org_set_workflow_permissions(org, workflow_permission = c("read", "write"))
```

## Arguments

- org:

  Character. Name of the GitHub organization(s).

- repo_permission:

  Default permission level members have for organization repositories:

  - read - can pull, but not push to or administer this repository.

  - write - can pull and push, but not administer this repository.

  - admin - can pull, push, and administer this repository.

  - none - no permissions granted by default.

- create_repositories:

  Logical. Can members create repositories.

- create_public_repositories:

  Logical. Can members create public repositories.

- create_private_repositories:

  Logical. Can members create private repositories.

- fork_private_repositories:

  Logical. Can members fork private repositories.

- create_teams:

  Logical. Can members create teams.

- workflow_permission:

  The default workflow permissions granted to the GITHUB_TOKEN when
  running workflows in the organization. Accepted values:`"read"` or
  `"write"`.

## Value

`org_sitep()` invisibly returns the `org` argument.

`org_set_repo_permission()` invisibly return a the result of the
relevant GitHub API call.

`org_set_permissions()` invisibly returns the result of the relevant
GitHub API call.

`org_workflow_permissions()` returns a character vector with value of
either `"read"` or `"write"`.

`org_set_workflow_permissions()` invisibly return a the result of the
relevant GitHub API call.

`org_allows_forking()` returns a logical scalar.

## Examples

``` r
if (FALSE) { # \dontrun{
org_sitrep("ghclass-test")

org_set_repo_permission("ghclass-test", "read")

org_set_permissions("ghclass-test", create_repositories = FALSE, create_teams = FALSE)

org_workflow_permissions("ghclass-test")

org_set_workflow_permissions("ghclass-test", "write")

org_sitrep("ghclass-test")

# Cleanup
org_set_repo_permission("ghclass-test", "none")
org_set_permissions("ghclass-test", create_repositories = TRUE, create_teams = TRUE)
org_set_workflow_permissions("ghclass-test", "read")
} # }
```
