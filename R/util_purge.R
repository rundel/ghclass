purge_all = function(org) {
  repos = org_repos(org)
  teams = org_teams(org)
  users = c( org_members(org, include_admins = FALSE), org_pending(org))

  if (length(repos) != 0)
    repo_delete(repos, prompt = TRUE)

  if (length(teams) != 0)
    team_delete(org, teams, prompt = TRUE)

  if (length(users) != 0)
    org_remove(org, users, prompt = TRUE)
}
