github_api_issue_comment = function(repo, number, body) {
  arg_is_chr_scalar(repo, body)

  ghclass_api_v3_req(
    endpoint = "POST /repos/:owner/:repo/issues/:issue_number/comments",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    issue_number = number,
    body = body
  )
}


#' @rdname issue
#'
#' @param comment Character. Optional comment to post on the issue before
#' closing it. If posting the comment fails the issue is not closed. Recycled
#' against `repo` / `number`.
#'
#' @export
#'
issue_close = function(repo, number, comment = NULL) {
  arg_is_chr(repo)
  arg_is_chr(comment, allow_null = TRUE)
  arg_is_pos_int(number)

  if (is.null(comment))
    comment = list(NULL)

  d = tibble::tibble(repo, number, comment)

  res = status_scope(
    "Closing issues", nrow(d),
    done = "Closed {n_ok} of {total} issue{?s}",
    purrr::pmap(
      d,
      function(repo, number, comment) {
        num_text = paste0("#", number)

        if (!is.null(comment)) {
          comment_res = purrr::safely(github_api_issue_comment)(
            repo, number, comment
          )

          if (failed(comment_res)) {
            status_msg(
              comment_res,
              fail = "Failed to comment on issue {.val {num_text}} for repo {.val {repo}}."
            )
            return(comment_res)
          }

          status_note("Commented on issue {.val {num_text}} for repo {.val {repo}}.")
        }

        close_res = purrr::safely(github_api_issue_edit)(
          repo, number, state = "closed"
        )

        status_msg(
          close_res,
          "Closed issue {.val {num_text}} for repo {.val {repo}}.",
          "Failed to close issue {.val {num_text}} for repo {.val {repo}}."
        )

        close_res
      }
    )
  )

  invisible(res)
}
