#' @rdname action
#' @param workflow_pat Character. Name of the workflow badge to be removed, or a regex pattern
#' that matches the workflow name.
#' @export
#'
action_remove_badge = function(repo, workflow_pat = ".*?", file = "README.md") {
  arg_is_chr(repo, workflow)
  arg_is_chr_scalar(file)

  res = purrr::map2(
    repo, workflow_pat,
    function(repo, workflow_pat) {
      pattern = glue::glue(
        "\\[!\\[{workflow_pat}\\]\\(.*?\\)\\]\\(https://github.com/.*?/actions.*?\\)\\s*"
      )

      repo_modify_file(
        repo = repo, path = file,
        pattern = pattern, content = "",
        method = "replace", all = TRUE
      )
    }
  )

  invisible(res)
}
