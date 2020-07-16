github_api_issue_create = function(repo, title, body, labels, assignees){
  ghclass_api_v3_req(
    endpoint = "POST /repos/:owner/:repo/issues",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    title = title,
    body = body,
    labels = I(labels),
    assignees = I(assignees)
  )
}


#' @rdname issue
#' @export
#'
issue_create = function(repo, title, body, labels = character(), assignees = character()) {

  arg_is_chr(repo, title, body)

  if (!is.list(labels))
    labels = list(labels)

  if (!is.list(assignees))
    assignees = list(assignees)

  res = purrr::pmap(
    list(repo, title, body, labels, assignees),
    function(repo, title, body, labels, assignees) {
      res = purrr::safely(github_api_issue_create)(
        repo, title, body, labels, assignees
      )

      status_msg(
        res,
        "Created issue {.val {title}} for repo {.val {repo}}.",
        "Failed to create issue {.val {title}} for repo {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}
