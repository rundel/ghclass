github_api_repo_edit = function(repo, ...) {
  arg_is_chr_scalar(repo)

  gh::gh(
    "PATCH /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    ...,
    .token = github_get_token(),
    # Needed for template repos
    .send_headers = c(Accept = "application/vnd.github.baptiste-preview+json")
  )
}
