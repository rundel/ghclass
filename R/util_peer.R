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

peer_label = function(repo, verbose = FALSE) {

  arg_is_chr(repo)

  name = c(":pencil: Give feedback", ":mag: View feedback")
  color = c("ff004d", "ffb400")
  desc = c("Please provide feedback", "Please view and rate feedback")

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


create_lastcommiturl = function(repo, path) {
  purrr::map2_chr(repo, path,
                  function(.x, .y) {
                    sub = get_commits(repo = .x, path = .y)
                    sub = sub[order(sub$date, decreasing = TRUE),]
                    glue::glue("https://github.com/{.x}/commit/{sub[1, 'sha']}")
                  })
}
