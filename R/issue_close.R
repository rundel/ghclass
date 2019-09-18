github_api_issue_edit = function(repo, number,
                                 title = NULL, body = NULL, state = NULL,
                                 milestone = NULL, labels = NULL,
                                 assignees =NULL) {
  args = list(
    endpoint = "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    issue_number = number,
    .token = github_get_token()
  )

  args[["title"]]     = title
  args[["body"]]      = body
  args[["state"]]     = state
  args[["milestone"]] = milestone
  args[["labels"]]    = labels
  args[["assignees"]] = assignees

  do.call(gh::gh, args)
}

#' Close an issue
#'
#' `issue_close` creates an issue for a GitHub repository.
#'
#' @param repo Character. Address of one or more repositories in `owner/name` format.
#' @param number Integer. Issue number of the issue to close.
#'
#' @export
#'
issue_close = function(repo, number) {

  arg_is_chr(repo)
  #FIXME
  #arg_is_int(number)

  res = purrr::map2(
    repo, number,
    function(repo, number) {
      res = purrr::safely(github_api_issue_edit)(
        repo, number, state = "closed"
      )

      num_text = paste0("#",number)
      status_msg(
        res,
        glue::glue("Closed issue {usethis::ui_value(num_text)} for repo {usethis::ui_value(repo)}."),
        glue::glue("Failed to close issue {usethis::ui_value(num_text)} for repo {usethis::ui_value(repo)}.")
      )

      res
    }
  )

  invisible(res)
}


#' Open an issue
#'
#' `issue_open` creates an issue for a GitHub repository.
#'
#' @param repo Character. Address of one or more repositories in `owner/name` format.
#' @param number Integer. Issue number of the issue to close.
#'
#' @export
#'
issue_open = function(repo, number) {

  arg_is_chr(repo)
  #FIXME
  #arg_is_int(number)

  res = purrr::map2(
    repo, number,
    function(repo, number) {
      res = purrr::safely(github_api_issue_edit)(
        repo, number, state = "open"
      )

      num_text = paste0("#",number)
      status_msg(
        res,
        glue::glue("Opened issue {usethis::ui_value(num_text)} for repo {usethis::ui_value(repo)}."),
        glue::glue("Failed to open issue {usethis::ui_value(num_text)} for repo {usethis::ui_value(repo)}.")
      )

      res
    }
  )

  invisible(res)
}
