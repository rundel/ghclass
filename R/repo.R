github_api_get_repo = function(repo) {
  stopifnot(length(repo) == 1)

  gh::gh(
    "GET /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = get_github_token()
  )
}

#' Check existence of GitHub repository
#'
#' `check_repo` returns TRUE if the github repository exists.
#' The function also prints a message if a repository has been renamed.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param strict Logical. Specifies whether renamed repositories are allowed.
#' @param verbose Logical. Specifies if details on renamed repositories should be printed.
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
#' @export
#'
check_repo = function(repo, strict = FALSE, verbose = TRUE) {

  arg_is_chr(repo)
  arg_is_lgl_scalar(strict, verbose)

  # Checking if repo exists
  repo_details = purrr::map(repo, purrr::safely(github_api_get_repo))
  repo_exists = purrr::map_lgl(repo_details, succeeded)

  cur_names = purrr::map_chr(repo_details, c("result","full_name"), .default = NA)
  cur_names = replace_nas(cur_names, repo)

  renamed = cur_names != repo

  if (verbose) {
    purrr::walk2(
      repo[renamed], cur_names[renamed],
      ~ usethis::ui_info("Repo {usethis::ui_value(.x)} has been renamed to {usethis::ui_value(.y)}.")
    )
  }

  if (strict)
    repo_exists = repo_exists & !renamed

  repo_exists
}



github_api_create_repo = function(repo, private, auto_init, gitignore_template){
  gh::gh("POST /orgs/:owner/repos",
         owner = get_repo_owner(repo),
         name = get_repo_name(repo),
         private = private,
         auto_init = auto_init,
         gitignore_template = gitignore_template,
         .token = get_github_token())
}

#' Create repository
#'
#' `create_repo` creates either individual or team repositories for a given
#' assignment.
#'
#' @param org Character. Name of the GitHub organization.
#' @param name Character. One or more GitHub username or team name.
#' @param prefix Character. Common repository name prefix
#' @param suffix Character. Common repository name suffix
#' @param private Logical. Create private repositories?
#' @param auto_init Logical. Initialize the repository with a README.md?
#' @param gitignore_template Character. .gitignore template language.
#'
#' @examples
#' \dontrun{
#' create_repo("ghclass-test", c("user01","user02"), prefix = "hw01-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
create_repo = function(org, name,
                       prefix = "", suffix = "",
                       private = TRUE, auto_init = FALSE,
                       gitignore_template = "R") {

  arg_is_chr(name)
  arg_is_chr_scalar(org, prefix, suffix, gitignore_template)
  arg_is_lgl_scalar(private, auto_init)

  if (prefix == "" & suffix == "")
    usethis::ui_stop("Either a prefix or a suffix must be specified.")

  org_repos = get_repos(org)

  repo = paste0(prefix, name, suffix)
  repo = fix_repo_name(repo)
  repo = paste0(org, "/", repo)

  purrr::walk(
    repo,
    function(repo) {
      if (repo %in% org_repos) {
        usethis::ui_info("Skipping repo {usethis::ui_value(repo)}, it already exists.")
        return()
      }
      res = github_api_create_repo(owner = get_repo_owner(repo),
                                   name = get_repo_name(repo),
                                   private = private,
                                   auto_init = auto_init,
                                   gitignore_template = gitignore_template)

      status_msg(
        purrr::safely(create_repo)(),
        usethis::ui_done("Created repo {usethis::ui_value(repo)}."),
        usethis::ui_oops("Failed to create repo {usethis::ui_value(repo)}.")
      )
    }
  )
}


github_api_add_user = function(repo, username, permission){
  gh::gh("PUT /repos/:owner/:repo/collaborators/:username",
         owner = get_repo_owner(repo),
         repo = get_repo_name(repo),
         username = username,
         permission = permission,
         .token = get_github_token())
}


#' Add a user or team to a repository
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param user Character. One or more GitHub usernames.
#' @param team Character. One or more GitHub team names.
#' @param permission Character. Permission to be granted to team for repo ("push", "pull", or "admin"), defaults to "push".
#'
#' `pull` results in read privileges, `push` in write privileges,
#' and `admin` in Admin privileges for the team in the respective repository.
#' Note that permissions will overwrite existing access privileges.
#'
#' @examples
#' \dontrun{
#' add_user_to_repo("ghclass-test/hw1-user01", c("user01", "user02"))
#' }
#'
#' @aliases add_team_to_repo
#' @aliases add_user_to_repo
#'
#' @rdname add_owner_to_repo
#'
#' @export
add_user_to_repo = function(repo, user,
                            permission = c("push", "pull", "admin")) {

  permission = match.arg(permission)
  arg_is_chr(repo, user)

  purrr::walk2(
    repo, user,
    function(repo, user) {

      if (verbose)
        message("Adding ", team, " to ", repo, " (", permission, ") ...")

      res = purrr::safely(github_api_add_use)(
        repo = repo,
        username = user,
        permission = permission
      )

      status_msg(
        res,
        usethis::ui_done("Added user {usethis::ui_value(user)} to repo {usethis::ui_value(repo)}."),
        usethis::ui_oops("Failed to add user {usethis::ui_value(user)} to repo {usethis::ui_value(repo)}.")
      )
    }
  )
}


github_api_add_team = function(repo, team_id, permission){
  gh::gh(
    "PUT /teams/:team_id/repos/:owner/:repo",
    team_id = team_id,
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    permission = permission,
    .token = get_github_token()
  )
}

#' @rdname add_owner_to_repo
#' @export
add_team_to_repo = function(repo, team,
                            permission = c("push", "pull", "admin")) {

  permission = match.arg(permission)
  arg_is_chr(repo, team)

  org = unique(get_repo_owner(repo))

  d = tibble::tibble(repo, team)
  d = team_id_lookup(d, get_specific_teams(org, team))

  purrr::pwalk(
    d,
    function(repo, team, id) {
      res = purrr::safely(github_api_add_team)(
        repo = repo,
        team_id = id,
        permission = permission
      )

      status_msg(
        res,
        usethis::ui_done(glue::glue("Added team {usethis::ui_value(team)} to repo {usethis::ui_value(repo)}.")),
        usethis::ui_oops(glue::glue("Failed to add team {usethis::ui_value(team)} to repo {usethis::ui_value(repo)}."))
      )
    }
  )
}





github_api_rename_repo = function(repo, new_name){
  gh::gh(
    "PATCH /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    name = new_name,
    .token = get_github_token()
  )
}

#' Rename repository
#'
#' `rename_repo` renames repositories. Use with caution as repositories retain their
#' unique identifier upon renaming and can be accessed under their old names due to
#' GitHub re-directing.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param new_name Character. New name of repository in the "name" format.
#'
#' @examples
#' \dontrun{
#' rename_repo("ghclass-test/hw1", "homework1")
#' }
#'
#' @export
#'
rename_repo = function(repo, new_name) {
  arg_is_chr(repo, new_name)

  purrr::walk2(
    repo, new_name,
    function(repo, new_name) {
      status_msg(
        purrr::safely(github_api_rename_repo)(repo, new_name),
        usethis::ui_done(glue::glue("Renamed repo {usethis::ui_value(repo)} to {usethis::ui_value(new_name)}.")),
        usethis::ui_oops(glue::glue("Failed to rename repo {usethis::ui_value(repo)} to {usethis::ui_value(new_name)}."))
      )
    }
  )
}



#' Mirror repository
#'
#' `mirror_repo` mirrors the content of one repository to another repository, or set of repositories. Use the `get_repos` function as a wrapper for the target_repo parameter to enable mirroring to multiple repositories.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. One or more repository addresses in "owner/name" format.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' mirror_repo("ghclass-test/hw1", c("ghclass-test/hw1-Team1", "ghclass-test/hw1-Team2"))
#' mirror_repo("ghclass-test/hw1", get_repos("ghclass-test","hw1-"))
#' }
#'
#' @export
#'
mirror_repo = function(source_repo, target_repo, verbose=FALSE)
{
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)

  withr::local_dir(tempdir())

  dir = clone_repo(source_repo, getwd(), options = "--bare", verbose = verbose)

  purrr::walk(
    target_repo,
    function(repo) {
      mirror_push_repo(dir, repo, verbose = verbose)
    }
  )

  unlink(file.path(dir), recursive = TRUE)
  usethis::ui_done("Removed local copy of {usethis::ui_value(source_repo)}")
}



github_api_create_pull = function(repo, base, head, title, body){
  safe_gh(
    "POST /repos/:owner/:repo/pulls",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    base = base, head = head, title = title, body = body,
    .token = get_github_token())
}

#' Create pull request
#'
#' `create_pull_request` creates a pull request on GitHub from the `base` branch to the `head` branch.
#'
#' @param repo Character. Address of one or more repositories in "owner/name" format.
#' @param title Character. Title of the pull request.
#' @param base Character. The name of the branch where your changes are implemented. In creating a pull request from a
#' fork then use `username:branch` as the format.
#' @param head Character. The branch you want the changed pulled into.
#' @param body Character. The text contents of the pull request.
#'
#' @export
#'
create_pull_request = function(repo, title, base, head = "master", body = "") {

  arg_is_chr(repo, title, base, head, body)

  purrr::pwalk(
    list(repo, base, head, title, body),
    function(repo, base, head, title, body) {
      res = purrr::safely(github_api_create_pull)(
        repo, base = base, head = head, title = title, body = body
      )

      details = glue::glue(
        "{usethis::ui_value(repo)}",
        "({usethis::ui_value(base)} ",
        "{usethis::ui_value(head)})"
      )

      status_msg(
        purrr::safely(github_api_rename_repo)(repo, new_name),
        usethis::ui_done(glue::glue("Created pull request for {details}.")),
        usethis::ui_oops(glue::glue("Failed create pull request for {details}."))
      )
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


github_api_get_collaborator = function(repo) {
  safe_gh(
    "GET /repos/:owner/:repo/collaborators",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = get_github_token(),
    .limit = get_github_api_limit()
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

# Deprecated functions ---------------------------------------------------------

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
