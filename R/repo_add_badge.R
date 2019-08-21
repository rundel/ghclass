#' Add a GitHub Action badge
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param workflow Character. Name of the workflow located in
#' `.github/workflows/` folder.
#' @param where Character. Regex pattern of where to insert the badge, defaults
#' to the beginning of the README
#' @param line_padding Character. What should be added to the end of the link,
#' defaults to sufficient new lines to add a blank line after the badge.
#'
#' @export
#'
repo_add_badge = function(repo, workflow, where = "^.", line_padding = "\n\n\n") {
  arg_is_chr(repo, workflow)
  arg_is_chr_scalar(where, line_padding)

  url = glue::glue("https://github.com/{repo}/workflows/{workflow}/badge.svg")
  url = URLencode(url)

  link = glue::glue(
    "[![{workflow} status]({url})](https://github.com/{repo}/actions){line_padding}"
  )

  repo_modify_file(repo, "README.md", where, content = link, method = "before")
}



#' Remove one or more GitHub Action badges
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param workflow Character. Name of the workflow badge to be removed, or a regex pattern that matches the workflow name.
#'
#' @export
#'
repo_remove_badge = function(repo, workflow = ".*?") {
  arg_is_chr(repo, workflow)

  pattern = glue::glue(
    "\\[!\\[{workflow} status\\]\\(.*?\\)\\]\\(https://github.com/.*?/actions\\)\\s*"
  )

  repo_modify_file(
    repo, "README.md",
    pattern, content = "", method = "replace", all = TRUE
  )
}
