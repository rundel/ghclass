github_api_issue_edit = function(
  repo, number,
  title = NULL, body = NULL, state = NULL, milestone = NULL,
  labels = list(), assignees = list()
) {
  ghclass_api_v3_req(
    endpoint  = "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner     = get_repo_owner(repo),
    repo      = get_repo_name(repo),
    issue_number = number,
    labels    = I(labels),
    assignees = I(assignees),
    title     = title,
    body      = body,
    state     = state,
    milestone = milestone
  )
}

# TODO - add issue_edit function
