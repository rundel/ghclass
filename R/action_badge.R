#' @name action_badge
#' @rdname action_badge
#'
#' @title Add or remove GitHub Actions badges from a repository
#'
#'
#' @description
#' * `action_add_badge` - Add a GitHub Actions badge to a file.
#'
#' * `action_remove_badge` - Remove one or more GitHub Action badges from a file.
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param workflow Character. Name of the workflow.
#' @param workflow_pat Character. Name of the workflow to be removed, or a regex pattern
#' that matches the workflow name.
#' @param where Character. Regex pattern indicating where to insert the badge, defaults
#' to the beginning of the target file.
#' @param line_padding Character. What text should be added after the badge.
#' @param file Character. Target file to be modified, defaults to `README.md`.#'
#'
NULL



#' @rdname action_badge
#' @export
#'
action_add_badge = function(repo, workflow = NULL, where = "^.",
                            line_padding = "\n\n\n", file = "README.md") {
  arg_is_chr(repo)
  arg_is_chr(workflow, allow_null=TRUE)
  arg_is_chr_scalar(where, line_padding, file)

  if (is.null(workflow)) {
    d = purrr::map_dfr(
      repo,
      ~ tibble::tibble(
        repo = .x,
        workflow = action_workflows(.x)[["name"]]
      )
    )
  } else {
    d = tibble::tibble(
      repo = repo,
      workflow = workflow
    )
  }

  d[["url"]] =  glue::glue_data(d, "https://github.com/{repo}/workflows/{workflow}/badge.svg")
  d[["dest"]] = glue::glue_data(d, "https://github.com/{repo}/actions?query=workflow:\"{workflow}\"")
  d[["link"]] = glue::glue_data(d, "[![{workflow}]({url_encode(url)})]({url_encode(dest)})")

  # Collapse by repo to save multiple changes to a single file
  d = dplyr::group_by(d, repo) %>%
    dplyr::summarize(
      link = paste0(paste(.data$link, collapse = " "), line_padding),
      workflows = list(workflow)
    )

  res = purrr::pmap(
    d,
    function(repo, link, workflows) {
      repo_modify_file(
        repo = repo, path = file,
        pattern = where, content = link,
        method = "before"
      )
    }
  )

  invisible(res)
}


#' @rdname action_badge
#' @export
#'
action_remove_badge = function(repo, workflow_pat = ".*?", file = "README.md") {
  arg_is_chr(repo, workflow_pat)
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
