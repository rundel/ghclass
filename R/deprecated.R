#' @export
clone_repo = function(...) {
  .Deprecated("local_repo_clone")
  local_repo_clone(...)
}

#' @export
add_repo = function(...) {
  .Deprecated("local_repo_add")
  local_repo_add(...)
}

#' @export
commit_repo = function(...) {
  .Deprecated("local_repo_commit")
  local_repo_commit(...)
}

#' @export
push_repo = function(...) {
  .Deprecated("repo_push")
  local_repo_push(...)
}

#' @export
pull_repo = function(...) {
  .Deprecated("local_repo_pull")
  local_repo_pull(...)
}

#' @export
rename_local_repo = function(...) {
  .Deprecated("local_repo_rename")
  local_repo_rename(...)
}

#' @export
get_repo = function(...) {
  .Deprecated("org_repos")
  org_repos(...)
}

#' @export
get_teams = function(...) {
  .Deprecated("org_teams")
  org_teams(...)
}

#' @export
get_admin = function(...) {
  .Deprecated("org_admins")
  org_admins(...)
}

#' @export
invite_user = function(...) {
  .Deprecated("org_invite")
  org_invite(...)
}

#' @export
get_member = function(...) {
  .Deprecated("org_members")
  org_members(...)
}

#' @export
get_pending_member = function(...) {
  .Deprecated("org_pending_members")
  org_pending_members(...)
}

#' @export
create_team = function(...) {
  .Deprecated("team_create")
  team_create(...)
}

#' @export
rename_team = function(...) {
  .Deprecated("team_rename")
  team_rename(...)
}

#' @export
add_team_member = function(...) {
  .Deprecated("team_invite")
  team_invite(...)
}

#' @export
get_team_members = function(...) {
  .Deprecated("team_members")
  team_members(...)
}

#' @export
get_pending_team_members = function(...) {
  .Deprecated("team_pending_members")
  team_pending_members(...)
}

#' @export
get_team_repos = function(...) {
  .Deprecated("team_repos")
  team_repos(...)
}

#' @export
create_repo = function(...) {
  .Deprecated("repo_create")
  repo_create(...)
}

#' @export
rename_repo = function(...) {
  .Deprecated("repo_rename")
  repo_rename(...)
}

#' @export
add_team_to_repo = function(...) {
  .Deprecated("repo_add_team")
  repo_add_team(...)
}

#' @export
add_user_to_repo = function(...) {
  .Deprecated("repo_add_user")
  repo_add_user(...)
}

#' @export
mirror_repo = function(...) {
  .Deprecated("repo_mirror")
  repo_mirror(...)
}

#' @export
get_collaborator = function(...) {
  .Deprecated("repo_collaborators")
  repo_collaborators(...)
}

#' @export
add_file = function(...) {
  .Deprecated("repo_add_file")
  repo_add_file(...)
}

#' @export
add_content = function(...) {
  .Deprecated("repo_modify_file")
  repo_modify_file(...)
}

#' @export
put_file = function(...) {
  .Deprecated("repo_put_file")
  repo_put_file(...)
}

#' @export
get_file = function(...) {
  .Deprecated("repo_get_file")
  repo_get_file(...)
}

#' @export
get_readme = function(...) {
  .Deprecated("repo_get_readme")
  repo_get_readme(...)
}

#' @export
create_branch = function(...) {
  .Deprecated("branch_create")
  branch_create(...)
}

#' @export
protect_branch = function(...) {
  .Deprecated("branch_protect")
  branch_protect(...)
}

#' @export
unprotect_branch = function(...) {
  .Deprecated("branch_unprotect")
  branch_unprotect(...)
}

#' @export
get_watching = function(...) {
  .Deprecated("repo_watching")
  repo_watching(...)
}

#' @export
ignore_repo = function(...) {
  .Deprecated("repo_ignore")
  repo_ignore(...)
}

#' @export
unwatch_repo = function(...) {
  .Deprecated("repo_unwatch")
  repo_unwatch(...)
}

#' @export
watch_repo = function(...) {
  .Deprecated("repo_watch")
  repo_watch(...)
}

#' @export
get_github_token = function(...) {
  .Deprecated("github_get_token")
  github_get_token(...)
}

#' @export
set_github_token = function(...) {
  .Deprecated("github_set_token")
  github_set_token(...)
}

#' @export
test_github_token = function(...) {
  .Deprecated("github_test_token")
  github_test_token(...)
}

#' @export
check_repo = function(...) {
  .Deprecated("repo_exists")
  repo_exists(...)
}

#' @export
check_user_exists = function(...) {
  .Deprecated("user_exists")
  user_exists(...)
}


