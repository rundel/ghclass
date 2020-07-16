github_api_pr_create = function(repo, head, base, title, body, draft = TRUE){
  ghclass_api_v3_req(
    endpoint = "POST /repos/:owner/:repo/pulls",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    head = head,
    base = base,
    title = title,
    body = body,
    draft = draft,
    .send_headers = c(Accept = "application/vnd.github.shadow-cat-preview+json")
  )
}

#' @rdname pr
#'
#' @export
#'
pr_create = function(repo, title, head, base = "master", body = "", draft = FALSE) {

  arg_is_chr(repo, title, base, head, body)
  arg_is_lgl(draft)

  purrr::pwalk(
    list(repo, base, head, title, body, draft),
    function(repo, base, head, title, body, draft) {
      res = purrr::safely(github_api_pr_create)(
        repo, base = base, head = head, title = title, body = body, draft = draft
      )

      details = cli_glue("{repo} ({base} <- {head})")

      status_msg(
        res,
        "Created pull request for {.val {details}}.",
        "Failed create pull request for {.val {details}}."
      )
    }
  )
}
