#' @rdname repo_file
#'
#' @param pattern Character. Regex pattern.
#' @param content Character. Content to be added to the file.
#' @param method Character. Should the content `replace` the matched pattern or be inserted `before` or `after` the match.
#' @param all Character. Should all instances of the pattern be modified (`TRUE`) or just the first (`FALSE`).
#'
#' @export
#'
repo_modify_file = function(repo, path, pattern, content, method = c("replace", "before", "after"), all = FALSE,
                            message = "Modified content", branch = NULL, verbose = TRUE) {
  arg_is_chr(repo, path, pattern, content, message)
  arg_is_chr(branch, allow_null=TRUE)

  method = match.arg(method)
  arg_is_chr_scalar(method)
  arg_is_lgl_scalar(all, verbose)

  if (is.null(branch))
    branch = list(NULL)

  res = purrr::pmap(
    list(repo, path, pattern, content, message, branch),
    function(repo, path, pattern, content, message, branch) {
      repo_txt = format_repo(repo, branch, path)

      res = modify_file(repo, path, pattern, content, method, all, message, branch)

      if (verbose) {
        status_msg(
          res,
          "Modified file {.val {repo_txt}}.",
          "Failed to modify file {.val {repo_txt}}."
        )
      }

      res
    }
  )

  invisible(res)
}

# Returns a purrr::safely() style result so callers can report the outcome
# with their own messages via status_msg()
modify_file = function(repo, path, pattern, content, method, all, message, branch) {
  cur_content = repo_get_file(repo = repo, path = path, branch = branch, quiet = TRUE)

  if (is.null(cur_content))
    return(failed_result("Unable to retrieve file."))

  if (!is.character(cur_content))
    return(failed_result("Cannot modify non-text file."))

  if (all) sub_func = gsub
  else     sub_func = sub

  pattern = paste0("(", pattern, ")")

  content = switch(
    method,
    replace = content,
    before  = paste0(content, "\\1"),
    after   = paste0("\\1", content),
    cli_stop("This shouldn't happen")
  )

  new_content = sub_func(pattern, content, cur_content)

  if (cur_content == new_content)
    return(failed_result("Unable to find pattern in file."))

  repo_put_file(repo = repo, path = path, content = new_content,
                message = message, branch = branch, verbose = FALSE)
}
