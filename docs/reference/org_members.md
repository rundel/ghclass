# Tools for managing organization membership

- `org_invite()` - invites user(s) to a GitHub organization.

- `org_remove()` - remove user(s) from an organization (and all teams
  within that organization).

- `org_members()` - returns a (filtered) vector of organization members.

- `org_pending`() - returns a (filtered) vector of pending organization
  members.

- `org_admins()` - returns a vector of repository administrators. In the
  case of a non-organization owner (e.g. a user account) returns the
  owner's login.

## Usage

``` r
org_admins(org)

org_invite(org, user)

org_members(org, filter = NULL, exclude = FALSE, include_admins = TRUE)

org_pending(org, filter = NULL, exclude = FALSE)

org_remove(org, user, prompt = TRUE)
```

## Arguments

- org:

  Character. Name of the GitHub organization(s).

- user:

  Character. GitHub username(s).

- filter:

  Character. Regular expression pattern for matching (or excluding)
  results

- exclude:

  Logical. Should entries matching the regular expression be excluded or
  included. Default `FALSE`.

- include_admins:

  Logical. Should admin users be included in the results. Default
  `TRUE`.

- prompt:

  Logical. Prompt before removing member from organization. Default
  `TRUE`.

## Value

`org_members()`, `org_pending()`, and `org_admins` all return a
character vector of GitHub account names.

`org_invite()` and `org_remove()` invisibly return a list containing the
results of the relevant GitHub API calls.

## Examples

``` r
if (FALSE) { # \dontrun{
# Org Details
org_admins("ghclass-test")

org_admins("rundel") # User, not an organization


# Org Membership - Invite, Status, and Remove
students = c("ghclass-anya", "ghclass-bruno", "ghclass-celine",
             "ghclass-diego", "ghclass-elijah","ghclass-francis")

org_invite("ghclass-test", students)

org_members("ghclass-test")

org_pending("ghclass-test")

org_remove("ghclass-test", students, prompt = FALSE)

org_pending("ghclass-test")
} # }
```
