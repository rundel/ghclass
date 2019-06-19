github_api_get_collaborator = function(repo) {

  safe_gh(
    "GET /repos/:owner/:repo/collaborators",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = get_github_token(),
    .limit = get_github_api_limit()
  )

}

github_api_get_repo_id = function(id) {
  safe_gh(
    "GET /repositories/:id",
    id = id,
    .token = get_github_token()
  )
}

#' Check existence of GitHub repository
#'
#' `check_repo` returns TRUE if the github repository exists. The function also returns a message if a repository was previously renamed.
#'
#' @param repo Character. Address of repository in "owner/name" format. Can be a vector or list of repository addresses.
#' @param redirect Logical. Specifies whether previous names of repositories should be considered. The default is FALSE, such that only current repository names will be considered as existing.
#'
#' @examples
#' \dontrun{
#' check_repo(c("rundel/ghclass", "rundel/ghclass_fake"))
#' }
#'
#' @return A logical vector
#'
#' @family github repo related functions
#'
#' @return logical
#'
#' @export
#'
check_repo = function(repo, redirect = F) {

  # Checking if repo exists
  res = purrr::map(repo, github_api_get_repo)
  repo_exists = purrr::map_lgl(res, succeeded)

  # Checking whether user-provided repo name is current
  id = purrr::map_int(res, c("result", "id"), .default = NA)
  repo_new_name = purrr::map_chr(purrr::map(id, github_api_get_repo_id), c("result", "name"), .default = NA)
  repo_old_name = get_repo_name(repo)

  # Replacing with F if user-provided repo name is NOT current
  if(redirect == F){
    repo_exists = ifelse(repo_exists & repo_new_name != repo_old_name, F, repo_exists)
  }

  # Messaging
  purrr::walk2(repo_new_name,
               repo_old_name,
               function(repo_new_name, repo_old_name)

                 if(repo_new_name != repo_old_name & !is.na(repo_new_name)){
                   repo_old_name = paste(get_repo_owner(repo), repo_old_name, sep = "/")
                   repo_new_name = paste(get_repo_owner(repo), repo_new_name, sep = "/")
                   message(paste("Repository", repo_old_name, "was previously renamed to", repo_new_name))
                 })


  # Output
  repo_exists
}


#' Fix repository names
#'
#' `fix_repo_name` replaces spaces in repository names with `_`. It also replaces non-alphanumeric characters and special characters other than `_`, `.`, or `-` with `-`.
#'
#' @param repo_name Character. Name of repository. Can be vector of list of characters.
#'
#' @examples
#' \dontrun{
#' fix_repo_name("base hw1")
#' fix_repo_name(c("base hw1", "final*draft"))
#' fix_repo_name(list("base hw1", "final*draft"))
#' }
#'
#' @return A character vector of repository names in the corrected format.
#'
fix_repo_name = function(repo_name)
{
  repo_name = stringr::str_replace_all(repo_name, " ", "_")
  stringr::str_replace_all(repo_name, "[^A-Za-z0-9_.-]+","-")
}


github_api_create_repo = function(owner, name, private, auto_init, gitignore_template){
  safe_gh("POST /orgs/:owner/repos",
          owner = owner,
          name = name, private = private,
          auto_init = auto_init,
          gitignore_template = gitignore_template,
          .token = get_github_token())
}

github_api_create_team_repo = function(owner, name, private, auto_init, gitignore_template, team_id){
  safe_gh("POST /orgs/:owner/repos",
          owner = owner,
          name = name, private = private,
          team_id = team_id,
          auto_init = auto_init,
          gitignore_template = gitignore_template,
          .token = get_github_token())
}

github_api_add_user = function(owner, repo, username, permission){
  safe_gh("PUT /repos/:owner/:repo/collaborators/:username",
          owner = owner,
          repo = repo,
          username = username,
          permission = permission,
          .token = get_github_token())
}

github_api_add_team = function(id, owner, repo, permission){
  safe_gh("PUT /teams/:id/repos/:owner/:repo",
          id = id,
          owner = owner,
          repo = repo,
          permission = permission,
          .token = get_github_token())
}


#' Create individual repositories
#'
#' `create_individual_repo` creates repositories for each student for a given
#' assignment.
#'
#' @param org Character. Name of the GitHub organization.
#' @param user Character or data frame. Listing one or more users.
#' @param prefix Character. Resulting repository name will start with this. character string.
#' @param suffix Character. Resulting repository name will end with this character string.
#' @param private Logical. Create private repositories.
#' @param verbose Logical. Display verbose output.
#' @param auto_init Logical. Initialize the repository with a README.md.
#' @param gitignore_template Character. .gitignore template language.
#'
#' @examples
#' \dontrun{
#' create_individual_repo("Sta523-Fa17", c("user01","user02"), prefix = "hw01-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
create_individual_repo = function(org, user, prefix = "", suffix = "",
                                  private = TRUE, verbose = TRUE,
                                  auto_init = FALSE, gitignore_template = "R") {
  if (prefix == "" & suffix == "")
    stop("Either a prefix or a suffix must be specified")

  org_users = get_members(org)
  org_repos = get_repos(org)

  purrr::walk(
    user,
    function(user) {
      repo_name = fix_repo_name(paste0(prefix, user, suffix))
      repo = paste0(org, "/", repo_name)

      if (repo %in% org_repos) {
        if (verbose)
          message("Skipping repo ", repo, ", already exists ...",)

        return()
      }

      if (verbose)
        message("Creating repo ", repo, " ...", sep="")

      try({
        github_api_create_repo(owner = org,
                               name = repo_name,
                               private = private,
                               auto_init = auto_init,
                               gitignore_template = gitignore_template)
      })

      try({
        github_api_add_user(owner = org,
                            repo = repo_name,
                            username = user,
                            permission = "push")
      })
    }
  )
}


#' Create team repositories
#'
#' `create_team_repo` creates repos for team(s).
#'
#' @param org Character. Name of the GitHub organization.
#' @param team Character or data frame. Vector of team names.
#' @param prefix Character. Resulting repo name will start with this character string.
#' @param suffix Character. Resulting repo name will end with this character string.
#' @param private Logical. Create private repos.
#' @param verbose Logical. Display verbose output.
#' @param auto_init Logical. Initialize the repository with a README.md.
#' @param gitignore_template Character. .gitignore template language.
#'
#' @examples
#' \dontrun{
#' create_team_repo("Sta523-Fa17", c("team01","team02"), prefix = "hw01-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
create_team_repo = function(org, team,  prefix = "", suffix = "",
                            private = TRUE, verbose = TRUE,
                            auto_init = FALSE, gitignore_template = "R") {
  org_teams = get_teams(org)

  if (is.character(team)) {
    team = merge(
      tibble::tibble(team = team), org_teams,
      by = "team", all.x = TRUE
    )
  }

  stopifnot(is.data.frame(team) & all( c("team","id") %in% names(team)))

  missing_ids = is.na(team[["id"]])
  if (any(missing_ids))
    stop("Unable to locate team(s): ", paste(team[["team"]][missing_ids], collapse = ", "), call. = FALSE)

  org_repos = get_repo(org)

  purrr::pwalk(
    unique(team),
    function(team, id) {
      repo_name = fix_repo_name( paste0(prefix, team, suffix) )
      repo = paste0(org, "/", repo_name)

      if (repo %in% org_repos) {
        message("Skipping repo ", repo, ", already exists ...")
        return()
      }

      if (verbose)
        message("Creating repo ", repo, " ...")

      # Create
      try({
      res = github_api_create_team_repo(owner = org,
                                        name = repo_name, private = private,
                                        team_id = id,
                                        auto_init = auto_init,
                                        gitignore_template = gitignore_template)
      })

      # Give time write access
      try({
      github_api_add_team(id = id,
                          owner = org,
                          repo = repo_name,
                          permission = "push")
      })

      check_result(res, sprintf("Failed to create team repo %s.", repo_name), verbose)
    }
  )
}

get_team_id_tbl = function(owner, team) {

  stopifnot(is.character(owner))
  stopifnot(length(owner) == 1)
  stopifnot(is.character(team))

  owner_teams = get_teams(owner)

  team = unique(team)

  team_tbl = merge(
    tibble::tibble(team = team), owner_teams,
    by = "team", all.x = TRUE
  )

  missing_ids = is.na(team_tbl[["id"]])
  if (any(missing_ids))
    stop(
      "Unable to locate team(s): ",
      paste(team_tbl[["team"]][missing_ids], collapse = ", "),
      " in ", owner, ".",
      call. = FALSE
    )

  team_tbl
}

#' Add a team to a repository
#'
#' `add_team_to_repo` adds a team to an existing repository. `pull` results in read privileges, `push` in write privileges, and `admin` in Admin privileges for the team in the respective repository. Note that permissions will overwrite existing access privileges.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param team Character or data frame. Vector of team names.
#' @param permission Character. Permission to be granted to team for repo ("push", "pull", or "admin"), defaults to "push".
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' add_team_to_repo("Sta523-Fa17/resources", c("Team1", "Team2"))
#' }
#'
#' @export
#'
add_team_to_repo = function(repo, team,
                            permission = c("push", "pull", "admin"),
                            verbose = TRUE) {

  stopifnot(is.character(repo))
  stopifnot(is.character(team))

  permission = match.arg(permission)

  purrr::walk2(
    repo, team,
    function(repo, team) {
      owner = get_repo_owner(repo)
      reponame = get_repo_name(repo)

      team_id = get_team_id_tbl(owner, team)

      if (verbose)
        message("Adding ", team, " to ", repo, " (", permission, ") ...")

      res = github_api_add_team(id = team_id[["id"]],
                                owner = owner,
                                repo = reponame,
                                permission = permission)

      check_result(res, sprintf("Failed to add %s to %s.", team, repo), verbose)
    }
  )
}

github_api_rename_repo = function(owner, repo, name){
  safe_gh("PATCH /repos/:owner/:repo",
          owner = owner,
          repo = repo,
          name = name,
          .token = get_github_token())
}

#' Rename repository
#'
#' `rename_repo` renames repositories. Use with caution as repositories retain their unique identifier upon renaming and can be accessed under their old names due to GitHub re-directing.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param new_name Character. New name of repository in the "name" format.
#'
#' @examples
#' \dontrun{
#' rename_repo("Sta523-Fa17/hw1", "homework1")
#' }
#'
#' @export
#'
rename_repo = function(repo, new_name) {
  purrr::walk2(
    repo, new_name,
    function(repo, new_name) {
      res = github_api_rename_repo(owner = get_repo_owner(repo),
                                   repo = get_repo_name(repo),
                                   name = new_name)

      check_result(
        res,
        sprintf("Failed to rename %s to %s.", repo, new_name),
        TRUE
      )
    }
  )
}



#' Mirror repository
#'
#' `mirror_repo` mirrors the content of one repository to another repository, or set of repositories. Use the `get_repos` function as a wrapper for the target_repo parameter to enable mirroring to multiple repositories.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character or vector of characters.  Address of repository in "owner/name" format.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' mirror_repo("Sta523/hw1_base", c("Sta523-Fa17/hw1-Team1", "Sta523-Fa17/hw1-Team2"))
#' mirror_repo("Sta523/hw1_base", get_repos("Sta523-Fa17","hw1-"))
#' }
#'
#' @export
#'
mirror_repo = function(source_repo, target_repo, verbose=TRUE)
{
  stopifnot(length(source_repo) == 1)
  stopifnot(length(target_repo) >= 1)

  stopifnot(check_repo(source_repo))
  stopifnot(all(check_repo(target_repo)))

  git = require_git()

  cur_dir = getwd()
  setwd(tempdir())
  on.exit({setwd(cur_dir)})

  if (verbose)
    message("Cloning source repo (", source_repo, ") ...")

  system(paste0(git, " clone --bare ", get_repo_url(source_repo)), intern = FALSE,
         wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  repo_dir = dir(pattern = "\\.git")
  stopifnot(length(repo_dir) == 1)
  setwd(repo_dir)

  purrr::walk(
    target_repo,
    function(repo) {

      if (verbose)
        message("Mirroring ", source_repo, " to ", repo," ...")

      try({
        system(paste0(git, " push --mirror ", get_repo_url(repo)), intern = FALSE,
               wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
      })
    }
  )

  if (verbose)
    message("Cleaning up ...\n")

  unlink(file.path("..",repo_dir), recursive = TRUE)
}


github_api_create_pull = function(owner, repo, base, head, title, body){
  safe_gh(
    "POST /repos/:owner/:repo/pulls",
    owner = owner,
    repo = repo,
    base = base, head = head, title = title, body = body,
    .token = get_github_token())
}


create_pull_request = function(repo, title, base, head = "master", body = "", verbose = TRUE) {

  stopifnot(!missing(repo))
  stopifnot(!missing(base))
  stopifnot(!missing(head))
  stopifnot(!missing(title))

  purrr::pwalk(
    list(repo, base, head, title, body),
    function(repo, base, head, title, body) {
      res = github_api_create_pull(owner = get_repo_owner(repo),
                                   repo = get_repo_name(repo),
                                   base = base, head = head, title = title, body = body)

      check_result(
        res,
        sprintf("Failed to create pull request for %s (%s => %s).", repo, base, head),
        verbose
      )
    }
  )
}


#' Style repository
#'
#' `style_repo` implements "non-invasive pretty-printing of R source code" of .R or .Rmd files within a repository using the `styler` package and adhering to `tidyverse` formatting guidelines.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param files Character or vector of characters. Names of .R and/or .Rmd files that styler should be applied to.
#' @param branch Character. Name of new branch to be created. Default is "styler".
#' @param base Character. Name of branch that contains the .R and/or .Rmd files to be reformatted.
#' @param create_pull_request Logical. If TRUE, a pull request is created from branch to base.
#' @param tag_collaborators Logical. If TRUE, a message with the repository collaborators is displayed.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' style_repo("Sta523-Fa17/base_hw1", files = c("hw1_sample.Rmd"))
#' }
#'
#' @export
#'
style_repo = function(repo, files = c("*.R","*.Rmd"), branch = "styler", base = "master",
                      create_pull_request = TRUE, tag_collaborators = TRUE,
                      git = require_git(), verbose = TRUE) {
  stopifnot(styler_available())
  stopifnot(length(repo) >= 1)

  dir = file.path(tempdir(),"styler")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  on.exit({
    unlink(file.path(dir), recursive = TRUE)
  })

  purrr::pwalk(
    list(repo, base, branch),
    function(repo, base, branch) {
      ## TODO add base to branch
      create_branch(repo, cur_branch = base, new_branch = branch)
      clone_repo(repo, local_path = dir, branch = branch)

      path = fs::path(dir, get_repo_name(repo))

      withr::local_dir(path)

      file_paths = unlist(purrr::map(files, ~ fs::dir_ls(path, recurse = TRUE, glob = .x)),
                          use.names = FALSE)




      msg = c("Results of running styler:\n", utils::capture.output( styler::style_file(file_paths) ))
      writeLines(msg, "commit_msg")

      system(paste0(git, " add ", paste0(file_paths, collapse = " ")),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " commit -F commit_msg"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " push"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      if (create_pull_request) {

        msg = paste(c(
          "This pull request contains the results of running the automated R code formating tool styler ",
          "on your repo. Styling is based on the tidyverse [R style guide](http://style.tidyverse.org)\n",
          "\n",
          "Click on the commit below to see details of recommended changes. It is not necessary that your ",
          "code cleanly pass these checks, but if there is a large number of significant changes suggested ",
          "you should review the style guide with an eye towards potentially improving your code formatting."
        ), collapse = "")

        if (tag_collaborators) {
          users = get_collaborator(repo)[[1]]
          msg = paste0(msg,"\n\n", paste0("@", users, collapse = ", "))
        }

        create_pull_request(
          repo, title = "styler revisions",
          base = base, head = branch,
          body = paste0(msg, collapse = "\n"),
          verbose = verbose
        )
      }
    }
  )
}



github_api_get_admin = function(owner){
  safe_gh("GET /orgs/:owner/members",
          owner = owner,
          role = "admin",
          .token = get_github_token(),
          .limit = get_github_api_limit())
}



#' List repository administrators
#'
#' `get_admin` creates a list of repository administrators.
#'
#' @param org Character. Name of GitHub organization.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' get_admin("Sta523-Fa17")
#' }
#'
#' @return A list containing a character vector of repository administrators.
#'
#' @export
#'
get_admin = function(org, verbose = FALSE) {

  purrr::map(
    org,
    function(org) {
      res = github_api_get_admin(owner = org)

      purrr::map_chr(res$result, "login")
    }
  )
}


#' List collaborator(s)
#'
#' `get_collaborator` Returns a vector of collaborator user names. Users with Admin rights are by default excluded, but can be included manually.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param include_admin Logical. If FALSE, user names of users with Admin rights are not included, defaults to TRUE.
#' @param verbose Logical. Display verbose output.
#'
#' @return A list containing a character vector of user names.
#'
#' @examples
#' \dontrun{
#' get_collaborators("Sta523-Fa17")
#' }
#'
#' @export
#'
get_collaborator = function(repo, include_admin = TRUE, verbose = FALSE) {

  stopifnot(!missing(repo))

  admin = list(NULL)
  if (!include_admin)
    admin = get_admin(get_repo_owner(repo))

  purrr::map2(
    repo, admin,
    function(repo, admin) {
      res = github_api_get_collaborator(repo)

      check_result(res, sprintf("Unable to retrieve collaborators for %s.", repo), verbose)

      setdiff(purrr::map_chr(res$result, "login"), admin)
    }
  )
}

################ Deprecated functions ################
github_api_get_collaborators = function(repo) {
  safe_gh(
    "GET /repos/:owner/:repo/collaborators",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = get_github_token(),
    .limit = get_github_api_limit()
  )
}

#' List repository collaborators
#'
#' `get_repo_collaborators` returns collaborator usernames.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#'
#' @return Character vector of collaborator usernames.
#'
#' @templateVar fun get_repo_collaborators
#' @template template-depr_fun
#'
#' @templateVar old get_repo_collaborators
#' @templateVar new get_collaborator
#' @template template-depr_pkg
#'
#' @examples
#' \dontrun{
#' get_repo_collaborators("Sta523-Fa17/hw1")
#' }
#'
get_repo_collaborators = function(repo) {

  .Deprecated(msg = "'get_repo_collaborators' is deprecated and will be removed in the next version. Use 'get_collaborators' instead.",
              new = "get_collaborator")

  users = purrr::map(
    repo,
    function(repo) {
      res = github_api_get_collaborators(repo)
      purrr::map_chr(res, "login")
    }
  )

  unique(unlist(users))
}

#' Check if repository exists
#'
#' @param repos Character. Address of repository in "owner/name" format.
#'
#' @templateVar fun check_repos
#' @template template-depr_fun
#'
#' @templateVar old check_repos
#' @templateVar new check_repo
#' @template template-depr_pkg
#'
#'
check_repos = function(repos)
{
  .Deprecated(msg = "'check_repos' is deprecated and will be removed in the next version. Use 'check_repo' instead.",
              new = "check_repo")

  exists = function(owner, repo)
  {
    gh("GET /repos/:owner/:repo", owner = owner, repo = repo, .token = get_github_token())
    TRUE
  }

  purrr::map2_lgl(
    get_repo_owner(repos), get_repo_name(repos),
    purrr::possibly(exists, FALSE)
  )
}


github_api_get_admins = function(org){
  safe_gh("GET /orgs/:org/members",
          org = org,
          role = "admin",
          .token = get_github_token(),
          .limit = get_github_api_limit())
}

#' List repository administrators
#'
#' `get_admins` creates a list of repository administrators.
#'
#' @param org Character. Name of GitHub organization.
#' @param verbose Logical. Display verbose output.
#'
#' @templateVar fun get_admins
#' @template template-depr_fun
#'
#' @templateVar old get_admins
#' @templateVar new get_admin
#' @template template-depr_pkg
#'
#' @examples
#' \dontrun{
#' get_admins("Sta523-Fa17")
#' }
#'
#' @return A list containing a character vector of repository administrators.
#'
#'
get_admins = function(org, verbose = FALSE) {

  .Deprecated(msg = "'get_admins' is deprecated and will be removed in the next version. Use 'get_admin' instead.",
              new = "get_admin")

  purrr::map(
    org,
    function(org) {
      res = github_api_get_admins(org = org)

      purrr::map_chr(res$result, "login")
    }
  )
}

#' List collaborators
#'
#' `get_collaborators` Returns a vector of collaborator user names. Users with Admin rights are by default excluded, but can be included manually.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param include_admins Logical. If FALSE, user names of users with Admin rights are not included. Default is TRUE.
#' @param verbose Logical. Display verbose output.
#'
#' @return A list containing a character vector of user names.
#'
#' @templateVar fun get_collaborators
#' @template template-depr_fun
#'
#' @templateVar old get_collaborators
#' @templateVar new get_collaborator
#' @template template-depr_pkg
#'
#' @examples
#' \dontrun{
#' get_collaborators("Sta523-Fa17")
#' }
#'
get_collaborators = function(repo, include_admins = TRUE, verbose = FALSE) {

  .Deprecated(msg = "'get_collaborators' is deprecated and will be removed in the next version. Use 'get_collaborator' instead.",
              new = "get_collaborator")

  stopifnot(!missing(repo))

  admins = list(NULL)
  if (!include_admins)
    admins = get_admins(get_repo_owner(repo))

  purrr::map2(
    repo, admins,
    function(repo, admins) {
      res = github_api_get_collaborators(repo)

      check_result(res, sprintf("Unable to retrieve collaborators for %s.", repo), verbose)

      setdiff(purrr::map_chr(res$result, "login"), admins)
    }
  )
}
