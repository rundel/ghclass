purge_all = function(org) {
  repos = org_repos(org)
  teams = org_teams(org)
  users = org_members(org, include_admins = FALSE)

  repo_delete(repos, prompt = TRUE)
  team_delete(org, teams, prompt = TRUE)
  org_remove(org, users, prompt = TRUE)
}
