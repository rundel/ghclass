github_api_issue_create = function(repo, title, body, assignee, label) {
  gh::gh(
    "POST /repos/:owner/:repo/issues",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    title = title,
    body = body,
    assignee = assignee,
    label = label,
    .token = github_get_token()
  )
}

github_api_label_create = function(repo, name, color, description) {
  gh::gh(
    "POST /repos/:owner/:repo/labels",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    name = name,
    color = color,
    description = description,
    .token = github_get_token()
  )
}

peer_create_label = function(repo, verbose = FALSE) {
  arg_is_chr(repo)

  name = c(":pencil: Complete review", ":mag: Inspect review")
  color = c("ff004d", "ffb400")
  desc = c("Please complete review", "Please inspect and rate review")

  purrr::pwalk(list(name, color, desc),
               function(name, color, desc) {
                 res = purrr::safely(github_api_label_create)(
                   repo = repo,
                   name = name,
                   color = color,
                   description = desc
                 )

                 if (verbose) {
                   status_msg(
                     res,
                     glue::glue("Created label {usethis::ui_value(name)}"),
                     glue::glue("Failed to create label {usethis::ui_value(name)}")
                   )
                 }
               })
}


peer_apply_label = function(org, filter = NULL, exclude = FALSE) {

  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl(exclude)

  repos = org_repos(org = org,
                    filter = filter,
                    exclude = exclude,
                    full_repo = TRUE)

  usethis::ui_info("This might take a moment...")
  purrr::walk(repos, ~ peer_create_label(.))
  usethis::ui_done("Applied peer review labels to all repositories in {usethis::ui_value(org)}.")

}

create_lastcommiturl = function(repo, path) {
  out = purrr::map2_dfc(repo, path,
                        function(.x, .y) {
                          sub = get_commits(repo = .x, path = .y)
                          sub = sub[order(sub$date, decreasing = TRUE), ]
                          paste0("https://github.com/", .x, "/commit/", sub[1, 'sha'])
                        })
  setNames(out, paste0("diff", seq_len(length(out))))
}

