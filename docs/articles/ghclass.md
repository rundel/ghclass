# Course management with ghclass

## What is ghclass?

ghclass is an R package that is designed to enable instructors to
efficiently manage their courses on GitHub. It has a wide range of
functionality for managing organizations, teams, repositories, and users
on GitHub and helps automate most of the tedious and repetitive tasks
around creating and distributing assignments.

## Who is this package for?

This package is for everyone! But really, if you’re an instructor who
uses GitHub for your class management, e.g. students submit assignments
via GitHub repos, this package is definitely for you! The package also
assumes that you’re familiar with R, but teaching with R is not a
requirement since this package is entirely agnostic to the contents of
your repositories.

(If you’re a Python user, see [this
post](https://ubc-mds.github.io/2017-08-24-teaching-with-github/) for a
Python-based alternative.)

## What is this article about?

This article is about the nitty-gritty of setting your class up in
GitHub with ghclass. For a higher level discussion of why and how to use
Git/GitHub in data science education, see [this
paper](https://doi.org/10.1080/00031305.2017.1397549) by the package
authors.

## Structuring your class on GitHub

The general framework is outlined below. This is not the only way to
structure a class on GitHub, but it’s a good way, and one that ghclass
is optimized to work with.

We outline steps for achieving this structure in the next section. This
section is meant to give a high level view of what your course looks
like on GitHub.

- One organization per class: If you teach at a university, this means
  one semester of a given course. If you teach workshops, this would be
  one workshop. The instructor and any additional instructional staff,
  e.g. teaching assistants, are owners. Going forward we will refer to
  this group of people as “instructors”. The students are members.

- One repo per student (or team) per assignment: The instructors have
  admin access to repos, i.e. they can clone, read, and write to every
  repository. Additionally, they can adjust repo and team memberships by
  adding or removing collaborators to assignment repositories as well as
  deleting them. The students have write access to their assigned repo,
  which means that they can clone, read, and write to their assigned
  repositories but they cannot delete them and they cannot add or remove
  collaborators to them. This can help with minimizing accidents that
  cannot be undone and makes sure students cannot peek into each other’s
  repositories unless you explicitly allow them to do so.

If you have a teamwork component to your course, you can also set up
teams on GitHub with your organization and each team can be given
similar repository level access privileges for team assignments.

Suppose you have 40 students in your class, and they are in 10 teams of
4 students each. Suppose also that students turn in the following
throughout a semester:

- Individual: 10 homework assignments + 2 exams
- Teamwork: 8 lab assignments + 1 project

Then, throughout the semester you will need to create a total of 570
repositories.

That is a lot of repos to create and permissions to set manually! It’s
also a lot of repos to clone when it comes time to grade. ghclass
addresses this problem, and more! It does not, however, address the
problem that that’s a lot of grading. Sorry, you’re on your own there!

That being said, ghclass does facilitate setting up continuous
integration tools using GitHub Actions for students’ assignment repos
(see below). This can allow for some automatic checking and feedback
each time students push to a repo.

## Authentication

This package uses GitHub personal access tokens for authentication with
[GitHub](https://github.com/settings/tokens). The token is read from the
`GITHUB_PAT` environment variable if it is set, and otherwise from the
credentials stored with
[`gitcreds::gitcreds_set()`](https://gitcreds.r-lib.org/reference/gitcreds_get.html),
see
[`?github_token`](https://rundel.github.io/ghclass/reference/github_token.md)
for details.

If this is your first time setting up a personal access token (PAT),
generate a token in the browser after logging into GitHub (Settings \>
Developer settings \> Personal access tokens \> Tokens (classic)) or use
[`usethis::create_github_token()`](https://usethis.r-lib.org/articles/git-credentials.html).
When creating the token make sure it has the `repo` and `admin:org`
scopes, along with `workflow` if you plan to add GitHub Actions
workflows to student repos and `notifications` if you plan to use
[`repo_watch()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
[`repo_ignore()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
or
[`repo_unwatch()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
and `delete_repo` if you plan to use
[`repo_delete()`](https://rundel.github.io/ghclass/reference/repo_core.md).
Note that
[`usethis::create_github_token()`](https://usethis.r-lib.org/reference/github-token.html)
does not select `admin:org` by default. These scopes apply to classic
tokens. Fine-grained tokens can also be used, but GitHub does not report
their permissions so
[`github_token_sitrep()`](https://rundel.github.io/ghclass/reference/github_token.md)
cannot check them, and GitHub does not support them for
[`repo_watch()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
[`repo_ignore()`](https://rundel.github.io/ghclass/reference/repo_notification.md),
or
[`repo_unwatch()`](https://rundel.github.io/ghclass/reference/repo_notification.md).

You can test that your token is working correctly using the
[`github_test_token()`](https://rundel.github.io/ghclass/reference/github_token.md)
function. If everything is working correctly you should see something
like the following:

\
[`github_test_token`](https://rundel.github.io/ghclass/reference/github_token.md)`(``)`\
`#> ✔ Your GitHub PAT authenticated correctly.`

If your token is not working you will see an error message like this
instead:

\
[`github_test_token`](https://rundel.github.io/ghclass/reference/github_token.md)`(``"badtoken"``)`\
`#> ✖ Your GitHub PAT failed to authenticate.`\
`#> └─GitHub API error (401): Bad credentials`\
`#>   └─ API docs: https://docs.github.com/rest`

[`github_token_sitrep()`](https://rundel.github.io/ghclass/reference/github_token.md)
provides a more detailed report on the token, including its type,
source, and granted scopes, and flags any of the scopes above that are
missing:

\
[`github_token_sitrep`](https://rundel.github.io/ghclass/reference/github_token.md)`(``)`\
`#> `\
`#> ── GitHub token sitrep: ────────────────────────────────────────────────────────`\
`#> • Token type: "classic personal access token"`\
`#> • Token source: "gh credential provider (for example, gitcreds)"`\
`#> • API url: "https://api.github.com"`\
`#> • Authenticated as: "rundel"`\
`#> • Rate limit: 4689 of 5000 requests remaining, resets at "18:10:42".`\
`#> • Recommended scopes:`\
`#>   • repo: TRUE`\
`#>   • admin:org: TRUE`\
`#>   • workflow: TRUE`\
`#>   • notifications: TRUE`\
`#>   • delete_repo: TRUE`

### GitHub Enterprise

ghclass can also be used with a GitHub Enterprise Server instance by
setting the `GITHUB_API_URL` environment variable to the instance’s REST
API endpoint, e.g. `https://github.example.edu/api/v3`, typically in a
project-level `.Renviron`. All API requests, clone and push URLs, and
badge links will then use that host. Tokens are looked up per host, so a
token for github.com and one for your Enterprise instance can be stored
side by side with
`gitcreds::gitcreds_set("https://github.example.edu")`.

## Step-by-step guide

Start with creating an organization on GitHub for the course. We
recommend using the course number, semester/quarter, and year in the
organization name, e.g. for a course numbered Sta 199 in Spring 2026,
you can use something like `Sta199-Sp26`. The exact format is not
critical, but being consistent is helpful so that you can keep track of
all of your different courses.

Private repositories have been free for all GitHub users and
organizations since 2019, so no special discount or plan is needed to
keep student work private. GitHub Education does still offer [benefits
for verified teachers](https://github.com/education/teachers), including
a free upgrade of your course organization to the GitHub Team plan.

This is an optional step, but one that many will want to do. Approval is
usually very quick, but it is not something you would want to do the
night before classes begin. Give yourself at least a week to be safe.

### Permissions

Each new GitHub organization defaults to repositories being readable by
all members, regardless of whether they are private or public. This is
clearly undesirable for most classroom settings.

Individual-level permissions can be set via the “People” tab on the
organization page. We recommend that the course instructor and any
teaching assistants be owners of the organization and that students be
members.

GitHub allows further permissions for accessing and changing
repositories to be set for each individual member or at the
organization-level (under Settings \> Member privileges). We suggest the
organization-level settings below.

*Member repository permissions*

- Base permissions: None
- Repository creation (both Public and Private): Disabled
- Repository forking: Disabled

*Admin repository permissions*

- Repository visibility change: Disabled
- Repository deletion and transfer: Disabled
- Issue deletion: Disabled

*Member team permissions*

- Allow members to create teams: Disabled

We can get a quick snapshot of our organization using the `org_sitrep`
function which reports on these settings as well as other important
details.

\
[`org_sitrep`](https://rundel.github.io/ghclass/reference/org_perm.md)`(``"ghclass-vignette"``)`\
`#> `\
`#> ── ghclass-vignette sitrep: ────────────────────────────────────────────────────`\
`#> • Admins: "mine-cetinkaya-rundel", "rundel", and "thereseanders"`\
`#> • Members: 0`\
`#> • Public repos: 0`\
`#> • Private repos: 0`\
`#> • Default repository permission: "read" <- Warning: members can currently view`\
`#> all repos in this org.`\
`#> • Default workflow permissions: "write"`\
`#> • Members can create public repos: TRUE`\
`#> • Members can create private repos: TRUE`\
`#> • Members can fork private repos: FALSE`\
`#> • Members can change repo visibility: TRUE`\
`#> • Members can delete repos: TRUE`\
`#> • Members can delete issues: FALSE`\
`#> • Members can create teams: TRUE`\
`#> • Members can invite outside collaborators: TRUE`

We can see that the current default repository permission setting is
“read”, with a helpful warning that this enables members to view all
repositories, and that members are currently able to create repositories
and teams. These settings can be changed within the Organization
Settings page under Member privileges. Alternatively, we can use ghclass
to apply the member repository and team settings directly with
`org_set_permissions` (the default repository permission on its own can
also be changed with `org_set_repo_permission`). The admin repository
permissions (visibility change, deletion and transfer, and issue
deletion) are read-only in the GitHub API, so these still need to be set
on the settings page.

\
[`org_set_permissions`](https://rundel.github.io/ghclass/reference/org_perm.md)`(`\
`  ``"ghclass-vignette"``,`\
`  repo_permission ``=`` ``"none"``,`\
`  create_repositories ``=`` ``FALSE``,`\
`  fork_private_repositories ``=`` ``FALSE``,`\
`  create_teams ``=`` ``FALSE`\
`)`\
`#> ✔ Set org "ghclass-vignette" permissions: repo_permission = none, create_repositories = FALSE, fork_private_repositories = FALSE, and create_teams = FALSE.`

After changing these settings we can once again check the org’s sitrep
and see that the warning is now resolved.

\
[`org_sitrep`](https://rundel.github.io/ghclass/reference/org_perm.md)`(``"ghclass-vignette"``)`\
`#> `\
`#> ── ghclass-vignette sitrep: ────────────────────────────────────────────────────`\
`#> • Admins: "mine-cetinkaya-rundel", "rundel", and "thereseanders"`\
`#> • Members: 0`\
`#> • Public repos: 0`\
`#> • Private repos: 0`\
`#> • Default repository permission: "none"`\
`#> • Default workflow permissions: "write"`\
`#> • Members can create public repos: FALSE`\
`#> • Members can create private repos: FALSE`\
`#> • Members can fork private repos: FALSE`\
`#> • Members can change repo visibility: TRUE`\
`#> • Members can delete repos: TRUE`\
`#> • Members can delete issues: FALSE`\
`#> • Members can create teams: FALSE`\
`#> • Members can invite outside collaborators: TRUE`

The sitrep also reports the organization’s default workflow permissions
for GitHub Actions, which can be changed with
`org_set_workflow_permissions`.

### Adding students to the organization

Next, collect your students’ GitHub usernames. You can do this using
your web form tool of choice (e.g. Google Forms, MS Forms, etc.) or via
a quiz or survey on your school’s learning management system (LMS). We
will assume that you are able to then read these data into an R data
frame.

For example, your roster file might look something like the following:

\
`roster`` ``=`` ``readr``::`[`read_csv`](https://readr.tidyverse.org/reference/read_delim.html)`(`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"roster.csv"``, package ``=`` ``"ghclass"``)`` ``)`

\
`roster`\
`#> # A tibble: 6 × 5`\
`#>   email              github          hw1        hw2        hw3       `\
`#>   <chr>              <chr>           <chr>      <chr>      <chr>     `\
`#> 1 anya@school.edu    ghclass-anya    hw1-team01 hw2-team01 hw3-team01`\
`#> 2 bruno@school.edu   ghclass-bruno   hw1-team02 hw2-team02 hw3-team02`\
`#> 3 celine@school.edu  ghclass-celine  hw1-team03 hw2-team03 hw3-team03`\
`#> 4 diego@school.edu   ghclass-diego   hw1-team01 hw2-team03 hw3-team02`\
`#> 5 elijah@school.edu  ghclass-elijah  hw1-team02 hw2-team01 hw3-team03`\
`#> 6 francis@school.edu ghclass-francis hw1-team03 hw2-team02 hw3-team01`

Here we are using the students’ school email addresses as unique
identifiers. We also have their GitHub usernames, and we have assigned
them to different teams for our three homework assignments. If you would
rather have ghclass form the teams for you, `team_roster` randomly
assigns the members of a roster to teams of a given size and adds the
team names as a new column.

Using the `roster` data frame, we can then invite the students to the
class’ organization. Each of these students will be notified via email
from GitHub asking them to join the `ghclass-vignette` organization.

\
[`org_invite`](https://rundel.github.io/ghclass/reference/org_members.md)`(``org ``=`` ``"ghclass-vignette"``, user ``=`` ``roster``$``github``)`\
`#> ✔ Invited user "ghclass-anya" to org "ghclass-vignette".`\
`#> ✔ Invited user "ghclass-bruno" to org "ghclass-vignette".`\
`#> ✔ Invited user "ghclass-celine" to org "ghclass-vignette".`\
`#> ✔ Invited user "ghclass-diego" to org "ghclass-vignette".`\
`#> ✔ Invited user "ghclass-elijah" to org "ghclass-vignette".`\
`#> ✔ Invited user "ghclass-francis" to org "ghclass-vignette".`

We now need to wait for the students to accept these invitations before
they will have access to the organization. We can check the status of
these acceptances using the
[`org_members()`](https://rundel.github.io/ghclass/reference/org_members.md)
and
[`org_pending()`](https://rundel.github.io/ghclass/reference/org_members.md)
functions to see which students have accepted or not accepted the
invitation.

\
[`org_members`](https://rundel.github.io/ghclass/reference/org_members.md)`(``"ghclass-vignette"``)`\
`#> [1] "mine-cetinkaya-rundel" "rundel"                "thereseanders"`

\
[`org_members`](https://rundel.github.io/ghclass/reference/org_members.md)`(``"ghclass-vignette"``, include_admins ``=`` ``FALSE``)`\
`#> character(0)`

\
[`org_pending`](https://rundel.github.io/ghclass/reference/org_members.md)`(``"ghclass-vignette"``)`\
`#> [1] "ghclass-anya"    "ghclass-bruno"   "ghclass-celine"  "ghclass-diego"  `\
`#> [5] "ghclass-elijah"  "ghclass-francis"`

After some time, some of the students will have accepted the invitation.

\
[`org_members`](https://rundel.github.io/ghclass/reference/org_members.md)`(``"ghclass-vignette"``, include_admins ``=`` ``FALSE``)`\
`#> [1] "ghclass-anya"    "ghclass-celine"  "ghclass-francis"`

\
[`org_pending`](https://rundel.github.io/ghclass/reference/org_members.md)`(``"ghclass-vignette"``)`\
`#> [1] "ghclass-bruno"  "ghclass-diego"  "ghclass-elijah"`

We can now see that Anya, Celine, and Francis have accepted the invite
and we are still waiting on Bruno, Diego, and Elijah. Gentle prodding
and reminder emails are often necessary to get all of the students into
the organization.

### Preparing an assignment

As described above, ghclass uses a workflow where each team or
individual is given access to a single repo for each assignment. To
create these repositories we use a single template repository which
contains all of the files necessary for the assignment. Generally, this
will consist of things like a `README.md` with instructions, a
scaffolded `qmd`, `Rmd`, or `R` file where the students will enter their
answers, and any other necessary support files (e.g. data, images,
support scripts, etc.). For an example of such a repository you can take
a look at [hw1](https://github.com/Sta323-Sp19/hw1) from a Statistical
Computing course offered at Duke in the Spring of 2019. Note that this
repository is public and viewable by anyone, but this is not necessary
for your template repository. We can view the contents of a repository
without cloning it using `repo_tree`,

\
[`repo_tree`](https://rundel.github.io/ghclass/reference/repo_file.md)`(``"Sta323-Sp19/hw1"``)`\
`#> Sta323-Sp19/hw1`\
`#> ├─.gitignore`\
`#> ├─README.md`\
`#> ├─fizzbuzz.png`\
`#> ├─hw1.Rmd`\
`#> ├─hw1.Rproj`\
`#> ├─hw1_whitelist.R`\
`#> └─wercker.yml`

Once you have created the repository and are ready to distribute it to
students there is one more suggested step - setting the repo’s template
status to `TRUE`. This is a GitHub specific detail, but doing so makes
it much more efficient to create copies of the repo for your students.
We can set this option under the repo’s Settings on GitHub, just check
the box labelled “Template Repository” on the main settings page, or use
the `repo_set_template` function. This status can also be checked with
ghclass using `repo_is_template`.

\
[`repo_is_template`](https://rundel.github.io/ghclass/reference/repo_core.md)`(``"Sta323-Sp19/hw1"``)`\
`#> [1] FALSE`

\
[`repo_set_template`](https://rundel.github.io/ghclass/reference/repo_core.md)`(``"Sta323-Sp19/hw1"``)`\
`#> ✔ Changed the template status of repo "Sta323-Sp19/hw1" to TRUE.`

\
[`repo_is_template`](https://rundel.github.io/ghclass/reference/repo_core.md)`(``"Sta323-Sp19/hw1"``)`\
`#> [1] TRUE`

### Distributing a team assignment

Once you have created your template repository, it is then a
straightforward process to create the team or individual repositories
for your students. The recommended process is to use the
`org_create_assignment` function, which is a high level function that
takes care of each of the underlying steps for you. To start we will
create the hw1 team-based assignment given the teams in `roster`.

\
[`org_create_assignment`](https://rundel.github.io/ghclass/reference/org_create_assignment.md)`(`\
`  org ``=`` ``"ghclass-vignette"``,`\
`  user ``=`` ``roster``$``github``,`\
`  repo ``=`` ``roster``$``hw1``,`\
`  team ``=`` ``roster``$``hw1``,`\
`  source_repo ``=`` ``"Sta323-Sp19/hw1"``,`\
`  private ``=`` ``TRUE`\
`)`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-team01".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-team02".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-team03".`\
`#> ✔ Created team "hw1-team01" in org "ghclass-vignette".`\
`#> ✔ Created team "hw1-team02" in org "ghclass-vignette".`\
`#> ✔ Created team "hw1-team03" in org "ghclass-vignette".`\
`#> ✔ Added user "ghclass-anya" to team "hw1-team01".`\
`#> ✔ Added user "ghclass-bruno" to team "hw1-team02".`\
`#> ✔ Added user "ghclass-celine" to team "hw1-team03".`\
`#> ✔ Added user "ghclass-diego" to team "hw1-team01".`\
`#> ✔ Added user "ghclass-elijah" to team "hw1-team02".`\
`#> ✔ Added user "ghclass-francis" to team "hw1-team03".`\
`#> ✔ Team "hw1-team01" given "push" access to repo "ghclass-vignette/hw1-team01"`\
`#> ✔ Team "hw1-team02" given "push" access to repo "ghclass-vignette/hw1-team02"`\
`#> ✔ Team "hw1-team03" given "push" access to repo "ghclass-vignette/hw1-team03"`

Based on the output we can see that multiple steps are involved in this
process:

1.  The repositories are created by mirroring the contents of
    “Sta323-Sp19/hw1” into the new repositories. The names of these
    repositories are given by the `repo` argument, and in this case
    match the team names.

2.  Each of the teams is created within the organization.

3.  The students are added to their assignment teams.

4.  Teams are added to the repositories with “push” permission, allowing
    them to write and make changes to the repo.

### Distributing an individual assignment

If instead of hw1 being a team assignment, we wanted to distribute it as
an individual assignment, we can also achieve this using the
`org_create_assignment` function by simply excluding the `team` argument
(and providing appropriate repo names).

\
[`org_create_assignment`](https://rundel.github.io/ghclass/reference/org_create_assignment.md)`(`\
`  org ``=`` ``"ghclass-vignette"``,`\
`  user ``=`` ``roster``$``github``,`\
`  repo ``=`` `[`paste0`](https://rdrr.io/r/base/paste.html)`(``"hw1-ind-"``, ``roster``$``github``)``,`\
`  source_repo ``=`` ``"Sta323-Sp19/hw1"``,`\
`  private ``=`` ``TRUE`\
`)`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-ind-ghclass-anya".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-ind-ghclass-bruno".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-ind-ghclass-celine".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-ind-ghclass-diego".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-ind-ghclass-elijah".`\
`#> ✔ Mirrored repo "Sta323-Sp19/hw1" to repo "ghclass-vignette/hw1-ind-ghclass-francis".`\
`#> ✔ User "ghclass-anya" given "push" access to repo "ghclass-vignette/hw1-ind-ghclass-anya"`\
`#> ✔ User "ghclass-bruno" given "push" access to repo "ghclass-vignette/hw1-ind-ghclass-bruno"`\
`#> ✔ User "ghclass-celine" given "push" access to repo "ghclass-vignette/hw1-ind-ghclass-celine"`\
`#> ✔ User "ghclass-diego" given "push" access to repo "ghclass-vignette/hw1-ind-ghclass-diego"`\
`#> ✔ User "ghclass-elijah" given "push" access to repo "ghclass-vignette/hw1-ind-ghclass-elijah"`\
`#> ✔ User "ghclass-francis" given "push" access to repo "ghclass-vignette/hw1-ind-ghclass-francis"`

The underlying process here is very similar with the only difference
being that we no longer need to create teams and instead add the users
directly to the repositories.

### Listing Repos

Once the repos are created we can interact with them using ghclass. One
of the most common needs is simply to list which repos exist within our
organization and/or select some subset of them.

\
[`org_repos`](https://rundel.github.io/ghclass/reference/org_details.md)`(``"ghclass-vignette"``)`\
`#> [1] "ghclass-vignette/hw1-ind-ghclass-anya"   `\
`#> [2] "ghclass-vignette/hw1-ind-ghclass-bruno"  `\
`#> [3] "ghclass-vignette/hw1-ind-ghclass-celine" `\
`#> [4] "ghclass-vignette/hw1-ind-ghclass-diego"  `\
`#> [5] "ghclass-vignette/hw1-ind-ghclass-elijah" `\
`#> [6] "ghclass-vignette/hw1-ind-ghclass-francis"`\
`#> [7] "ghclass-vignette/hw1-team01"             `\
`#> [8] "ghclass-vignette/hw1-team02"             `\
`#> [9] "ghclass-vignette/hw1-team03"`

\
[`org_repos`](https://rundel.github.io/ghclass/reference/org_details.md)`(``"ghclass-vignette"``, filter``=``"hw1-team"``)`\
`#> [1] "ghclass-vignette/hw1-team01" "ghclass-vignette/hw1-team02"`\
`#> [3] "ghclass-vignette/hw1-team03"`

### Modifying Repos

Mirroring repos is somewhat heavy-handed, since it forces the target
repo to be identical to the source repo. In some cases we only want to
add or modify a single file in the repository. Most often this occurs
after distributing an assignment and discovering that there is an issue
with the instructions, the data, etc.

ghclass allows you to automate the process of adding, modifying, or
replacing files across repos after they have been created. While this
process does overwrite existing files in the repo, everything is being
done within the context of Git and changes can be rolled back or merged
if they conflict.

Let’s assume that we distributed hw1 with the wrong version of the
`README.md` included. If we want to replace this with the correct
version across all of the hw1 repositories, we could do the following:

\
`file`` ``=`` `[`system.file`](https://rdrr.io/r/base/system.file.html)`(``"hw1/README.md"``, package ``=`` ``"ghclass"``)`\
\
[`repo_add_file`](https://rundel.github.io/ghclass/reference/repo_file.md)`(`\
`  `[`org_repos`](https://rundel.github.io/ghclass/reference/org_details.md)`(``"ghclass-vignette"``,``"hw1-team"``)``,`\
`  message ``=`` ``"Replace README.md with the correct version"``,`\
`  file ``=`` ``file``,`\
`  overwrite ``=`` ``TRUE`\
`)`\
`#> ✔ Added file "README.md" to repo "ghclass-vignette/hw1-team01".`\
`#> ✔ Added file "README.md" to repo "ghclass-vignette/hw1-team02".`\
`#> ✔ Added file "README.md" to repo "ghclass-vignette/hw1-team03".`

The updated version looks like the following:

\
[`repo_get_readme`](https://rundel.github.io/ghclass/reference/repo_file.md)`(``"ghclass-vignette/hw1-team01"``, include_details ``=`` ``FALSE``)`\
`#> [1] "## Homework 01\n\nThis is the corrected version of the HW01 Readme\n"`

We can also use the function `repo_modify_file` to make changes to
existing files:

\
[`repo_modify_file`](https://rundel.github.io/ghclass/reference/repo_file.md)`(`\
`  repo ``=`` `[`org_repos`](https://rundel.github.io/ghclass/reference/org_details.md)`(``"ghclass-vignette"``,``"hw1-team"``)``,`\
`  path ``=`` ``"README.md"``,`\
`  pattern ``=`` ``"## Homework 01\n\n"``,`\
`  content ``=`` ``"Due: Tomorrow\n"``,`\
`  method ``=`` ``"after"`\
`)`\
`#> ✔ Modified file "ghclass-vignette/hw1-team01/README.md".`\
`#> ✔ Modified file "ghclass-vignette/hw1-team02/README.md".`\
`#> ✔ Modified file "ghclass-vignette/hw1-team03/README.md".`

\
[`repo_get_readme`](https://rundel.github.io/ghclass/reference/repo_file.md)`(``"ghclass-vignette/hw1-team01"``, include_details ``=`` ``FALSE``)`\
`#> [1] "## Homework 01\n\nDue: Tomorrow\nThis is the corrected version of the HW01 Readme\n"`

### Collecting Student Work

Eventually the students will be finished with the work and/or the
assignment deadline will have passed. ghclass makes it easy to collect
all of the student work from GitHub and make it accessible on your local
computer for grading. We make use of the
[gert](https://github.com/r-lib/gert) package to provide basic git
functionality within ghclass.

\
`repo_dir`` ``=`` `[`file.path`](https://rdrr.io/r/base/file.path.html)`(`[`tempdir`](https://rdrr.io/r/base/tempfile.html)`(``)``, ``"hw1"``)`

\
[`local_repo_clone`](https://rundel.github.io/ghclass/reference/local_repo.md)`(`\
`  repo ``=`` `[`org_repos`](https://rundel.github.io/ghclass/reference/org_details.md)`(``"ghclass-vignette"``, ``"hw1-team"``)``,`\
`  local_path ``=`` ``repo_dir`\
`)`\
`#> ✔ Cloned "ghclass-vignette/hw1-team01".`\
`#> ✔ Cloned "ghclass-vignette/hw1-team02".`\
`#> ✔ Cloned "ghclass-vignette/hw1-team03".`

\
`fs``::`[`dir_tree`](https://fs.r-lib.org/reference/dir_tree.html)`(``repo_dir``)`\
`#> /var/folders/v7/wrxd7cdj6l5gzr0191__m9lr0000gr/T//RtmpUxMLUl/hw1`\
`#> ├── hw1-team01`\
`#> │   ├── README.md`\
`#> │   ├── fizzbuzz.png`\
`#> │   ├── hw1.Rmd`\
`#> │   ├── hw1.Rproj`\
`#> │   ├── hw1_whitelist.R`\
`#> │   └── wercker.yml`\
`#> ├── hw1-team02`\
`#> │   ├── README.md`\
`#> │   ├── fizzbuzz.png`\
`#> │   ├── hw1.Rmd`\
`#> │   ├── hw1.Rproj`\
`#> │   ├── hw1_whitelist.R`\
`#> │   └── wercker.yml`\
`#> └── hw1-team03`\
`#>     ├── README.md`\
`#>     ├── fizzbuzz.png`\
`#>     ├── hw1.Rmd`\
`#>     ├── hw1.Rproj`\
`#>     ├── hw1_whitelist.R`\
`#>     └── wercker.yml`

### Automated checking with GitHub Actions

If your template repository includes a workflow under
`.github/workflows/`, for example one that renders the students’
document or runs a set of checks, then GitHub Actions will run it each
time a student pushes to their repository. ghclass provides several
tools for working with these workflows across many repositories:

- `org_create_assignment(add_badges = TRUE)` adds a status badge for
  each workflow to the README of every assignment repo so students can
  see whether their most recent push succeeded (see also
  `action_add_badge` and `action_remove_badge`).
- `action_workflows` and `action_runs` report on the workflows and their
  runs across repos, and `action_runtime` reports how long the runs
  took.
- `action_artifacts` and `action_artifact_download` list and download
  the artifacts produced by the workflows, e.g. rendered html or pdf
  output.
- `org_grade_assignment` combines these steps for grading: it clones all
  repos matching a filter, downloads the requested artifacts (skipping
  any that are out of sync with the current commit), and creates a
  comment file for each repo.

For example, to set up grading for hw1 and collect the rendered html
artifacts from each repo:

\
[`org_grade_assignment`](https://rundel.github.io/ghclass/reference/org_grade_assignment.md)`(`\
`  path ``=`` ``"hw1-grading"``,`\
`  org ``=`` ``"ghclass-vignette"``,`\
`  repo_filter ``=`` ``"hw1-team"``,`\
`  artifacts ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"html"`` ``=`` ``"html-output"``)`\
`)`

Note that adding or modifying workflow files in student repos requires
the `workflow` scope on your token.

## FAQ

1.  Do I really need private repositories for my students’ assignments?\
    \
    You might not care, but the law might. For example, in the United
    States, FERPA regulations stipulate that student information should
    be kept private. If you use public repositories, anyone can find out
    who is enrolled in your course. Additionally, you will likely be
    using GitHub issues for providing feedback on the students’ work,
    and potentially even mention their grade in a given assignment. This
    information should not be publicly available to anyone.\
    \
    Also, your students may not want their coursework to be publicly
    available. They are bound to make mistakes as they learn and it
    should be up to them whether they want those to be a piece of their
    public profile on GitHub.

2.  What about GitHub Classroom?\
    \
    GitHub retired GitHub Classroom on August 28, 2026 (see
    [here](https://github.com/orgs/community/discussions/205975) for the
    announcement). Organizations, repositories, and user accounts
    created through Classroom were not affected, so existing classroom
    organizations can continue to be managed with ghclass, which works
    directly with the GitHub API. The workflow described above covers
    the distribution of individual and team assignments, and GitHub
    Actions workflows can provide the automatic checking and feedback
    that Classroom’s autograding did.

3.  Does the default branch of my repository matter (`master` vs
    `main`)?\
    \
    Yes and no - since October 2020 GitHub has used `main` as the
    default branch for new repositories (see
    [here](https://github.com/github/renaming) for background), while
    repositories and organizations created before then kept `master`
    unless they were changed. ghclass supports either name across the
    entire package. For the vast majority of use cases you will not see
    any differences as the GitHub API and/or Git will already use the
    default branch without any additional specification. In a small
    number of cases a branch name is required, in which case the package
    does not provide a default value and you will be prompted to specify
    that argument.\
    \
    If your classroom org is already using `master` you can leave it as
    is, particularly for repos already distributed to students. For new
    organizations, the default branch name for newly created
    repositories can be set under
    `Org > Settings > Repository Defaults > Repository default branch`.
    Note that this only affects newly created repositories, not existing
    ones.
