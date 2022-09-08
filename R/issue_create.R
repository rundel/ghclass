github_api_issue_create = function(repo, title, body, labels=character(), assignees=character()){
  arg_is_chr_scalar(repo, title, body)
  arg_is_chr(labels, assignees)

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
#'
#' @param delay Numeric. Delay between each API request. Issue creation has a secondary rate limit (~ 20/min).
#'
#' @export
#'
issue_create = function(repo, title, body, labels = character(), assignees = character(), delay=0) {

  arg_is_chr(repo, title, body)

  if (!is.list(labels))
    labels = list(labels)

  if (!is.list(assignees))
    assignees = list(assignees)

  # Handle any necessary recycling
  df = tibble::tibble(
    repo, title, body, labels, assignees
  )

  n = nrow(df)

  if (n > 20 & delay == 0) {
    cli::cli_alert_info(c(
      "Attempting to create {.val {n}} issues - this is likely to trigger GitHub's secondary rate limit.",
      "Setting a delay of 5 seconds per request to avoid this, override this using {.field delay}."
    ), wrap=TRUE)

    delay = 5
  }

  res = purrr::pmap(
    df,
    function(repo, title, body, labels, assignees) {
      res = purrr::safely(github_api_issue_create)(
        repo, title, body, labels, assignees
      )

      status_msg(
        res,
        "Created issue {.val {title}} for repo {.val {repo}}.",
        "Failed to create issue {.val {title}} for repo {.val {repo}}."
      )

      Sys.sleep(delay)

      res
    }
  )

  invisible(res)
}
