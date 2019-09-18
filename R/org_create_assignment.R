#' Create a team or individual assignment
#'
#' This is a higher level function that combines the following steps:
#'
#' 1. Create repos
#' 2. Create teams and invite students if necessary
#' 3. Add teams or individuals to the repositories
#' 4. Mirror a template repository to assignment repositories
#'
#' @param org Character. Name of the GitHub organization.
#' @param repo Character. Name of the repo(s) for the assignment.
#' @param user Character. GitHub username(s).
#' @param team Character. Team names, if `NULL`` an individual assignment will be created.
#' @param source_repo Character. Address of the repository to use as a template for all created repos.
#'
#' @export
#'
org_create_assignment = function(org, repo, user, team = NULL, source_repo = NULL) {

  arg_is_chr_scalar(org)
  arg_is_chr(repo, user)
  arg_is_chr(team, allow_null = TRUE)
  arg_is_chr_scalar(source_repo, allow_null = TRUE)

  repo_create(org, repo)
  repos = paste0(org, "/", repo)

  if (!is.null(team)) {
    team_invite(org, user, team)
    repo_add_team(repos, team)
  } else {
    repo_add_user(repos, user)
  }

  if (!is.null(source_repo)) {
    repo_mirror(source_repo, repos)
  }
}