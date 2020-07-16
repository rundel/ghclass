#' @title Create a team or individual assignment
#'
#' @description
#' This is a higher level function that combines the following steps:
#'
#' * Create repos
#' * Create teams and invite students if necessary
#' * Add teams or individuals to the repositories
#' * Mirror a template repository to assignment repositories
#'
#' @param org Character. Name of the GitHub organization.
#' @param repo Character. Name of the repo(s) for the assignment.
#' @param user Character. GitHub username(s).
#' @param team Character. Team names, if not provided an individual assignment will be created.
#' @param source_repo Character. Address of the repository to use as a template for all created repos.
#' @param private Logical. Should the created repositories be private.
#'
#' @export
#'

org_create_assignment = function(org, repo, user, team = NULL, source_repo = NULL,
                                 private = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(repo, user)
  arg_is_chr(team, allow_null = TRUE)
  arg_is_chr_scalar(source_repo, allow_null = TRUE)
  arg_is_lgl_scalar(private)

  repos = paste0(org, "/", repo)

  existing = repo_exists(repos)
  if (any(existing)) {
    cli_stop(
      "The following repo{?s} already exist{?s/}: {.val {repos[existing]}}. ",
      "Delete these repo{?s} or use an alternative method to create the assignment."
    )
  }

  if (!is.null(source_repo) && repo_is_template(source_repo)) {
    repo_mirror_template(source_repo, repos, private = private)
  } else {
    repo_create(org, repo, private = private)
    if (!is.null(source_repo)) {
      repo_mirror(source_repo, repos, overwrite = TRUE)
    }
  }

  if (!is.null(team)) {
    # Assume team assignment
    team_create(org, unique(team))
    team_invite(org, user, team)
    repo_add_team(repos, team)
  } else {
    # Assume individual assignment
    repo_add_user(repos, user)
  }
}
