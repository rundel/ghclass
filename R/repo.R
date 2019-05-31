#' Check whether repository exists
#'
#' \code{check_repo} checks whether a given repository exists.
#'
#' @param repos character, list, or vector, repo names in the form of \emph{owner/name}
#'
#' @examples
#' \dontrun{
#' check_repo(c("ghclass-test/hw1", "ghclass-test/hw1-Team1"))
#' }
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @family github organization related functions
#'
#' @export
#'
check_repo = function(repos)
{
  exists = function(owner, repo)
  {
    gh("GET /repos/:owner/:repo", owner=owner, repo=repo, .token=get_github_token())
    TRUE
  }

  purrr::map2_lgl(
    get_repo_owner(repos), get_repo_name(repos),
    purrr::possibly(exists, FALSE)
  )
}

#' Fix repository names
#'
#' \code{fix_repo_name} replaces spaces in repo names with \code{_}. It also replaces non-alphanumeric characters and special characters other than \code{_}, \code{.}, or \code{-} with \code{-}.
#'
#' @param repos character, repo names in the form of \emph{owner/name}. Can be a vector or list of  names.
#'
#' @examples
#' \dontrun{
#' fix_repo_name("ghclass-test/base hw1")
#'
#' @export
fix_repo_name = function(repos)
{
  repos = stringr::str_replace_all(repos, " ", "_")
  stringr::str_replace_all(repos, "[^A-Za-z0-9_.-]+","-")
}

#' Create individual repositories
#'
#' \code{create_individual_repo} creates repos for each student for a given
#' assignment
#'
#' @param org character, name of the GitHub organization.
#' @param user character or data frame, listing one or more users
#' @param prefix character, resulting repo name will start with this character string
#' @param suffix character, resulting repo name will end with this character string
#' @param private logical, create private repos
#' @param verbose logical, display verbose output
#' @param auto_init logical, initialize the repository with a README.md
#' @param gitignore_template character, .gitignore template language
#'
#' @examples
#' \dontrun{
#' create_individual_repo("ghclass-test",c("user01","user02"), prefix="hw01-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
create_individual_repo = function(org, user, prefix="", suffix="",
                                  private=TRUE, verbose=TRUE,
                                  auto_init=FALSE, gitignore_template="R") {
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

      # Create repo
      try({
        gh("POST /orgs/:org/repos",
           org = org,
           name=repo_name, private=private,
           auto_init=auto_init,
           gitignore_template=gitignore_template,
           .token=get_github_token())
      })

      # Give time write access
      try({
        gh("PUT /repos/:owner/:repo/collaborators/:username",
           owner = org, repo = repo_name, username = user,
           permission="push",
           .token=get_github_token())
      })
    }
  )
}


#' Create team repositories
#'
#' \code{create_team_repo} creates repos for team(s)
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, vector of team names
#' @param prefix character, resulting repo name will start with this character string
#' @param suffix character, resulting repo name will end with this character string
#' @param private logical, create private repos
#' @param verbose logical, display verbose output
#' @param auto_init logical, initialize the repository with a README.md
#' @param gitignore_template character, .gitignore template language
#'
#' @examples
#' \dontrun{
#' create_team_repo("ghclass-test",c("team01","team02"), prefix="hw01-")
#' }
#'
#' @family github organization related functions
#'
#' @export
create_team_repo = function(org, team,  prefix="", suffix="",
                            private=TRUE, verbose=TRUE,
                            auto_init=FALSE, gitignore_template="R") {
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
    stop("Unable to locate team(s): ", paste(team[["team"]][missing_ids], collapse=", "), call. = FALSE)

  org_repos = get_repos(org)

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

      res = purrr::safely(function() {
        # Create repo
        gh("POST /orgs/:org/repos",
           org = org,
           name=repo_name, private=private, team_id=id,
           auto_init=auto_init, gitignore_template=gitignore_template,
           .token=get_github_token())

        # Give time write access
        gh("PUT /teams/:id/repos/:org/:repo",
           id = id, org = org, repo = repo_name,
           permission="push",
           .token=get_github_token())
      })()

      check_result(res, sprintf("Failed to create team repo %s.", repo_name), verbose)
    }
  )
}

#' Access unique team ids
#'
#' \code{get_team_id_tbl} returns a (filtered) data frame of teams in the organization with columns for their names (`team`) and their unique ids (`id`).
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, vector of team names
#'
#' @examples
#' \dontrun{
#' get_team_id_tbl("ghclass-test", c("Team1", "Team2"))
#' }
#'
#' @return A data frame with two columns and the number of rows equivalent to the length of the vector `team`
#'
#' @export
get_team_id_tbl = function(org, team) {

  stopifnot(is.character(org))
  stopifnot(length(org) == 1)
  stopifnot(is.character(team))

  org_teams = get_teams(org)

  team = unique(team)

  team_tbl = merge(
    tibble::tibble(team = team), org_teams,
    by = "team", all.x = TRUE
  )

  missing_ids = is.na(team_tbl[["id"]])
  if (any(missing_ids))
    stop(
      "Unable to locate team(s): ",
      paste(team_tbl[["team"]][missing_ids], collapse=", "),
      " in ", org, ".",
      call. = FALSE
    )

  team_tbl
}

#' Adding a team to existing repository
#'
#' \code{add_team_to_repo} adds a team to an existing repository. `pull` results in read privileges, `push` in write privileges, and `admin` in Admin privileges for the team in the respective repository. Note that permissions will overwrite existing access privileges.
#'
#' @param repo character, existing repo name in the form of \emph{owner/name}
#' @param team character or data frame, vector of existing team names
#' @param permission character vector, permissions to be granted to team for repo ("pull", "push", or "admin"), default is "pull"
#' @param verbose logical, display verbose output
#'
#' @examples
#' \donotrun{
#' add_team_to_repo("ghclass-test/resources", c("Team1", "Team2))
#' }
#'
#' @export
add_team_to_repo = function(repo, team,
                            permission = c("pull", "push", "admin"),
                            verbose=TRUE) {

  stopifnot(is.character(repo))
  stopifnot(is.character(team))

  permission = match.arg(permission)

  purrr::walk2(
    repo, team,
    function(repo, team) {
      org = get_repo_owner(repo)
      reponame = get_repo_name(repo)

      team_id = get_team_id_tbl(org, team)

      if (verbose)
        message("Adding ", team, " to ", repo, " (", permission, ") ...")

      res = safe_gh(
        "PUT /teams/:id/repos/:org/:repo",
        id = team_id[["id"]], org = org, repo = reponame,
        permission = permission,
        .token=get_github_token()
      )

      check_result(res, sprintf("Failed to add %s to %s.", team, repo), verbose)
    }
  )
}



#' Rename existing repository
#'
#' \code{rename_repo} renames existing repositories.
#'
#' @param repo character, or character vector, existing repo name in the form of \emph{owner/name}
#' @param newname character, or character vector, of new repository name
#' @param verbose logical, display verbose output
#'
#' @examples
#' \donotrun{
#' rename_repo("ghclass-test/resources", "literature")
#' }
#'
#'   @export
rename_repo = function(repo, new_name, verbose = T) {

  stopifnot(is.character(repo))
  stopifnot(is.character(new_name))

  stopifnot(all(check_repo(repo)))

  # Adding error handling
  if(length(repo) > length(new_name))
    stop("Provided more repos than names to be applied ...")

  purrr::walk2(
    repo, new_name,
    function(repo, new_name) {
      res = safe_gh("PATCH /repos/:owner/:repo",
                    owner = get_repo_owner(repo),
                    repo = get_repo_name(repo),
                    name = new_name,
                    .token=get_github_token())

      if (verbose)
        message("Renaming ", repo, " to ", new_name, " ...")

      check_result(
        res,
        sprintf("Failed to rename %s to %s.", repo, new_name),
        verbose
      )
    }
  )
}




#' Mirror repository
#'
#' @param source_repo repo name of source in the form of \emph{owner/name}
#' @param target_repos repo names of target in the form of \emph{owner/name}
#' @param verbose logical, display verbose output
#'
#' @export
mirror_repo = function(source_repo, target_repos, verbose=TRUE)
{
  stopifnot(length(source_repo) == 1)
  stopifnot(length(target_repos) >= 1)

  stopifnot(check_repo(source_repo))
  stopifnot(all(check_repo(target_repos)))

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
    target_repos,
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







create_pull_request = function(repo, title, base, head = "master", body = "", verbose = TRUE) {

  stopifnot(!missing(repo))
  stopifnot(!missing(base))
  stopifnot(!missing(head))
  stopifnot(!missing(title))

  purrr::pwalk(
    list(repo, base, head, title, body),
    function(repo, base, head, title, body) {
      res = safe_gh(
        "POST /repos/:owner/:repo/pulls",
        owner = get_repo_owner(repo), repo = get_repo_name(repo),
        base = base, head = head, title = title, body = body,
        .token = get_github_token()
      )

      check_result(
        res,
        sprintf("Failed to create pull request for %s (%s => %s).", repo, base, head),
        verbose
      )
    }
  )
}





#' @export
style_repo = function(repo, files=c("*.R","*.Rmd"), branch="styler", base="master",
                      create_pull_request = TRUE, tag_collaborators = TRUE,
                      git = require_git(), verbose=TRUE) {
  stopifnot(styler_available())
  stopifnot(length(repo) >= 1)

  dir = file.path(tempdir(),"styler")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  on.exit({
    unlink(file.path(dir), recursive = TRUE)
  })

  purrr::walk2(
    repo, branch,
    function(repo, branch) {
      ## TODO add base to branch
      branch_repo(repo, branch, verbose = FALSE)
      path = clone_repo(repo, local_path = dir, branch = branch)

      file_paths = unlist(purrr::map(files, ~ fs::dir_ls(path, recursive = TRUE, glob = .x)),
                          use.names = FALSE)

      cur_dir = getwd()
      setwd(path)

      on.exit({
        setwd(cur_dir)
      })

      msg = c("Results of running styler:\n", utils::capture.output( styler::style_file(file_paths) ))
      writeLines(msg, "commit_msg")

      system(paste0(git, " add ", paste0(file_paths, collapse=" ")),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " commit -F commit_msg"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " push"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      if (create_pull_request) {

        msg = paste(c(
          "This pull request contains the results of running the automated R code formating tool styler ",
          "on your repo. Styling is based on Hadley's [R style guide](http://adv-r.had.co.nz/Style.html)\n",
          "\n",
          "Click on the commit below to see details of recommended changes. It is not necessary that your ",
          "code cleanly pass these checks, but if there is a large number of significant changes suggested ",
          "you should review the style guide with an eye towards potentially improving your code formatting."
        ), collapse="")

        if (tag_collaborators)
          msg = paste0(msg,"\n\n@", get_collaborators(repo)[[1]], collapse=", ")

        create_pull_request(
          repo, title="styler revisions",
          base = base, head = branch,
          body = paste0(msg, collapse="\n"),
          verbose = verbose
        )
      }
    }
  )
}

#' @export
get_admins = function(org, verbose = FALSE) {

  purrr::map(
    org,
    function(org) {
      res = gh(
        "GET /orgs/:org/members",
        org = org,
        role = "admin",
        .token = get_github_token(),
        .limit = get_github_api_limit()
      )

      purrr::map_chr(res, "login")
    }
  )
}

#' @export
get_collaborators = function(repo, include_admins = FALSE, verbose = FALSE) {

  stopifnot(!missing(repo))

  admins = list(NULL)
  if (!include_admins)
    admins = get_admins(get_repo_owner(repo))

  purrr::map2(
    repo, admins,
    function(repo, admins) {
      res = safe_gh(
        "GET /repos/:owner/:repo/collaborators",
        owner = get_repo_owner(repo), repo = get_repo_name(repo),
        affiliation = "all",
        .token = get_github_token(),
        .limit = get_github_api_limit()
      )

      check_result(res, sprintf("Unable to retrieve collaborators for %s.", repo), verbose)

      setdiff(purrr::map_chr(res$result, "login"), admins)
    }
  )
}
