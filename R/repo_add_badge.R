#' Add a GitHub Actions badge
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param workflow Character. Name of the workflow located in `.github/workflows/`.
#' @param where Character. Regex pattern of where to insert the badge, defaults
#' to the beginning of the README
#' @param line_padding Character. What should be added to the end of the link,
#' defaults to sufficient new lines to add a blank line after the badge.
#' @param file Character. Target file to be modified, defaults to `README.md`.
#'
#' @export
#'
repo_add_badge = function(repo, workflow = NULL, where = "^.", line_padding = "\n\n\n", file = "README.md") {
  arg_is_chr(repo)
  arg_is_chr(workflow, allow_null=TRUE)
  arg_is_chr_scalar(where, line_padding, file)

  if (is.null(workflow)) {
    d = purrr::map_dfr(
      repo,
      ~ tibble::tibble(
        repo = .x,
        workflow = repo_workflows(.x)[["name"]]
      )
    )
  } else {
    d = tibble::tibble(
      repo = repo,
      workflow = workflow
    )
  }

  d[["url"]] =  glue::glue_data(d, "https://github.com/{repo}/workflows/{workflow}/badge.svg")
  d[["dest"]] = glue::glue_data(d, "https://github.com/{repo}/actions?query=workflow:{workflow}")
  d[["link"]] = glue::glue_data(d, "[![{workflow}]({url_encode(url)})]({url_encode(dest)})")

  # Collapse by repo to save multiple changes to a single file
  d = dplyr::summarize(
    dplyr::group_by(d, repo),
    link = paste0(paste(link, collapse = " "), line_padding),
    workflows = list(workflow)
  )

  res = purrr::pmap(
    d,
    function(repo, link, workflows) {
      repo_modify_file(
        repo = repo, path = "README.md",
        pattern = where, content = link,
        method = "before"
      )
    }
  )

  invisible(res)
}



#' Remove one or more GitHub Action badges
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param workflow Character. Name of the workflow badge to be removed, or a regex pattern
#' that matches the workflow name.
#' @param file Character. Target file to be modified, defaults to `README.md`.
#'
#' @export
#'
repo_remove_badge = function(repo, workflow = ".*?", file = "README.md") {
  arg_is_chr(repo, workflow)
  arg_is_chr_scalar(file)

  res = purrr::map2(
    repo, workflow,
    function(repo, workflow) {
      pattern = glue::glue(
        "\\[!\\[{workflow}\\]\\(.*?\\)\\]\\(https://github.com/.*?/actions.*?\\)\\s*"
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
