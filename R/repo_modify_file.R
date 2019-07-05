#' Modify a file within a repository
#'
#' @param repo Character. Address of repository in `owner/name`` format.
#' @param file Character. File's path within the repository.
#' @param content Character. Content to be added to the file.
#' @param after Character. Regex pattern, if not `NULL` content will be inserted directly after the first match.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to "master".
#'

#' @aliases add_content
#'
#' @export
#'
repo_modify_file = function(repo, file, content, after = NULL, message = "Added content", branch = "master") {
  arg_is_chr(repo, file, content, message, branch)
  arg_is_chr(after, allow_null = TRUE)

  purrr::pwalk(
    list(repo, file, content, after, message, branch),
    function(repo, file, content, after, message, branch) {

      cur_content = repo_get_file(repo, file, branch)

      if (is.null(cur_content)) {
        usethis::ui_oops(
          "Unable to retrieve {usethis::ui_value(format_repo(repo, branch, file))}."
        )
      } else {

        if (!is.null(after)) {
          pat_loc = regexec(after, cur_content)
          if (length(pat_loc) == 0) {
            usethis::ui_oops(
              "Unable to find pattern {usethis::ui_value(after)} in {usethis::ui_value(format_repo(repo, branch, file))}."
            )
            return(NULL)
          }

          split_loc =  pat_loc[[1]][[1]] +  attr(pat_loc[[1]], "match.length")

          content = paste0(
            substr(cur_content, 1, split_loc-1),
            content,
            substr(cur_content, split_loc, nchar(cur_content))
          )
        }

        res = repo_put_file(repo, file, content, message, branch, verbose = FALSE)

        status_msg(
          res,
          glue::glue("Modified file {usethis::ui_value(format_repo(repo, branch, file))}."),
          glue::glue("Failed to modify file {usethis::ui_value(format_repo(repo, branch, file))}."),
        )
      }
    }
  )
}
