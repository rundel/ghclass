#' ghclass: A package for github based classroom and assignment management
#'

#' @section Local git repository functions:
#'
#' * [local_repo_clone()] or
#'   [repo_clone()]        - (previously [clone_repo()]) -
#' * [local_repo_add()]    - (previously [add_repo()]) -
#' * [local_repo_commit()] - (previously [commit_repo()]) -
#' * [local_repo_push()]   - (previously [push_repo()]) -
#' * [local_repo_pull()]   - (previously [pull_repo()]) -
#' * [local_repo_rename()] - (previously [rename_local_repo()]) -
#'
#'
#' @section Organization functions:
#'
#' General functions,
#' * [org_repos()]           - (previously [get_repo()])
#' * [org_teams()]           -
#' * [org_team_ids()]        - (previously [get_teams()])
#' * [org_admins()]          - (previously [get_admin()])
#' * [org_invite()]          - (previously [invite_user()])
#' * [org_members()]         - (previously [get_member()])
#' * [org_pending_members()] - (previously [get_pending_member()])
#'
#' Team related functions,
#' * [team_create()]          - (previously [create_team()])
#' * [team_rename()]          - (previously [rename_team()])
#' * [team_invite()]          - (previously [add_team_member()])
#' * [team_members()]         - (previously [get_team_members()])
#' * [team_pending_members()] - (previously [get_pending_team_members()])
#' * [team_repos()]           - (previously [get_team_repos()])
#'
#'
#' @section GitHub repository functions:
#'
#' General functions,
#' * [repo_create()]        - (previously [create_repo()])
#' * [repo_rename()]        - (previously [rename_repo()])
#' * [repo_add_team()]      - (previously [add_team_to_repo()])
#' * [repo_add_user()]      - (previously [add_user_to_repo()])
#' * [repo_mirror()]        - (previously [mirror_repo()])
#' * [repo_collaborators()] - (previously [get_collaborator()])
#'
#' File manipulation functions,
#' * [repo_add_file()]    - (previously [add_file()])
#' * [repo_modify_file()] -
#' * [repo_put_file()]    - (previously [put_file()])
#' * [repo_get_file()]    - (previously [get_file()])
#' * [repo_get_readme()]  - (previously [get_readme()])
#'
#' Branch functions,
#' * [branch_create()]    - (previously [create_branch()])
#' * [branch_protect()]   - (previously [protect_branch()])
#' * [branch_unprotect()] - (previously [unprotect_branch()])
#'
#' Notification functions:
#' * [repo_watching()] - (previously [get_watching()] -)
#' * [repo_ignore()]   - (previously [ignore_repo()] -)
#' * [repo_unwatch()]  - (previously [unwatch_repo()] -)
#' * [repo_watch()]    - (previously [watch_repo()])
#'
#'
#' @section GitHub authentication functions:
#'
#' * [github_get_token()]  - (previously [get_github_token()])
#' * [github_set_token()]  - (previously [set_github_token()])
#' * [github_test_token()] - (previously [test_github_token()])
#'
#'
#' @section Utility functions:
#'
#' * [repo_exists()] - (previously [check_repo()])
#' * [user_exists()] - (previously [check_user_exists()])
#'
#' @import gh
#'
#' @docType package
#' @name ghclass
NULL


