github_api_issue_edit = function(
  repo, number,
  title = NULL, body = NULL, state = NULL, milestone = NULL,
  labels = list(), assignees = list()
) {

  args = list(
    endpoint = "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    issue_number = number,
    labels = I(labels),
    assignees = I(assignees),
    .token = github_get_token()
  )

  args[["title"]]     = title
  args[["body"]]      = body
  args[["state"]]     = state
  args[["milestone"]] = milestone

  do.call(gh::gh, args)
}

# TODO - add issue_edit function
