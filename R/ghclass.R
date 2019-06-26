#' ghclass: A package for github based classroom and assignment management
#'
#' @section GitHub repositoryy functions:
#'
#' * [create_repo()] -
#' * [add_team_to_repo()] -
#' * [add_user_to_repo()] -
#' * [mirror_repo()] -
#' * [rename_repo()] -
#'
#' @section Local git repository functions:
#'
#' * [clone_repo()] -
#' * [add_repo()] -
#' * [commit_repo()] -
#' * [push_repo()] -
#' * [pull_repo()] -
#' * [rename_local_repo()] -
#'
#'
#' @section Organization functions:
#'
#' * [get_repo()] -
#' * [get_admin()] -
#' * [get_collaborator()] -
#' * [get_member()] -
#' * [get_pending_member()] -
#' * [invite_user()] -
#'
#'
#' @section Team functions:
#'
#' * [get_teams()] -
#' * [create_team()] -
#' * [rename_team()] -
#' * [add_team_member()] -
#' * [get_team_members()] -
#' * [get_pending_team_members()] -
#' * [get_team_repos()] -
#'
#'
#' @section GitHub authentication functions:
#'
#' * [get_github_token()] - get github token
#' * [set_github_token()] - set github token
#' * [test_github_token()] - test github token
#'
#'
#' @section Repository file functions:
#'
#' * [add_file()] -
#' * [add_content()] -
#' * [get_file()] -
#' * [get_readme()] -
#' * [put_file()] -
#'
#'
#' @section Branch functions:
#'
#' * [create_branch()] -
#' * [create_pull_request()] -
#' * [protect_branch()] -
#' * [unprotect_branch()] -
#'
#'
#' @section Notification functions:
#'
#' * [get_watching()] -
#' * [ignore_repo()] -
#' * [unwatch_repo()] -
#' * [watch_repo()] -
#'
#'
#' @section Utililty functions:
#'
#' * [check_repo()] -
#' * [check_user_exists()] -
#'
#'
#' @section Experimental functions:
#'
#' * [style_repo()] - use `styler` on all R code within a repo and create a branch + pull request with the result
#'
#'
#' @import gh
#'
#' @docType package
#' @name ghclass
NULL

