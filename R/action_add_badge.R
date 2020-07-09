#' @rdname action
#' @param workflow Character. Name of the workflow located in `.github/workflows/`.
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
      link = paste0(paste(link, collapse = " "), line_padding),
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

