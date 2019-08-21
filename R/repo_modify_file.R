#' Modify a file within a repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param file Character. File's path within the repository.
#' @param pattern Character. Regex pattern.
#' @param content Character. Content to be added to the file.
#' @param method Character. Should the content be `insert`ed after the match or should it `replace` the matched string.
#' @param all Character. Should all instances of the pattern be modified (`TRUE`) or just the first (`FALSE`).
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to "master".
#'

#' @aliases add_content
#'
#' @export
#'
repo_modify_file = function(repo, file, pattern, content, method = c("replace", "before", "after"), all = FALSE,
                            message = "Modified content", branch = "master") {
  arg_is_chr(repo, file, pattern, content, message, branch)

  method = match.arg(method)
  arg_is_chr_scalar(method)
  arg_is_lgl_scalar(all)

  purrr::pwalk(
    list(repo, file, pattern, content, message, branch),
    function(repo, file, pattern, content, message, branch) {

      cur_content = repo_get_file(repo, file, branch)

      if (is.null(cur_content)) {
        usethis::ui_oops(
          "Unable to retrieve {usethis::ui_value(format_repo(repo, branch, file))}."
        )
      } else {

        if (all) sub_func = gsub
        else     sub_func = sub

        pattern = paste0("(", pattern, ")")

        content = switch(
          method,
          replace = content,
          before  = paste0(content, "\\1"),
          after   = paste0("\\1", content),
          stop("This shouldn't happen")
        )

        new_content = sub_func(pattern, content, cur_content)

        if (cur_content == new_content) {
          usethis::ui_oops(
            "Unable to find pattern {usethis::ui_value(pattern)} in {usethis::ui_value(format_repo(repo, branch, file))}."
          )
          return(NULL)
        }

        res = repo_put_file(repo, file, new_content, message, branch, verbose = FALSE)

        status_msg(
          res,
          glue::glue("Modified file {usethis::ui_value(format_repo(repo, branch, file))}."),
          glue::glue("Failed to modify file {usethis::ui_value(format_repo(repo, branch, file))}."),
        )
      }
    }
  )
}
