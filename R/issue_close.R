#' @rdname issue
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
        "Closed issue {.val {num_text}} for repo {.val {repo}}.",
        "Failed to close issue {.val {num_text}} for repo {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}
