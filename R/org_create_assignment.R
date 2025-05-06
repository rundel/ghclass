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
#' @param add_badges Logical. Should GitHub action badges be added to the README.
#'
#' @return An invisible list containing the results of each step.
#'
#' @export
#'

org_create_assignment = function(org, repo, user, team = NULL, source_repo = NULL,
                                 private = TRUE, add_badges = FALSE) {

  arg_is_chr_scalar(org)
  arg_is_chr(repo, user)
  arg_is_chr(team, allow_null = TRUE)
  arg_is_chr_scalar(source_repo, allow_null = TRUE)
  arg_is_lgl_scalar(private)

  repo_full = paste0(org, "/", repo)

  if (!is.null(source_repo) && !repo_is_template(source_repo)) {
    expr = paste0("repo_set_template(", source_repo, ")")
    cli_stop(
      "{.val {source_repo}} is not a template repo - ",
      "this can be set using {.code repo_set_template({.val {source_repo}})}}."
    )
  }

  existing = repo_exists(repo_full)
  if (any(existing)) {
    cli_stop(
      "The following repo{?s} already exist{?s/}: {.val {repo_full[existing]}}. ",
      "Delete these repo{?s} or use an alternative method to create the assignment."
    )
  }

  res = list()

  if (!is.null(source_repo)) {
    res[["mirror"]] = repo_mirror_template(source_repo, repo_full, private = private)
  } else {
    repo_create(org, repo, private = private)
  }

  if (!is.null(team)) {
    # Assume team assignment
    res[["team_create"]] = team_create(org, unique(team))
    res[["team_invite"]] = team_invite(org, user, team)
    res[["team_add"]] = repo_add_team(repo_full, team)
  } else {
    # Assume individual assignment
    res[["user_invite"]] = repo_add_user(repo_full, user)
  }

  if (add_badges)
    res[["add_badge"]] = action_add_badge(unique(repo_full))

  invisible(res)
}
