#' @rdname action_badge
#'
#' @note `action_add_badge()` is not idempotent; re-running it adds another badge
#' block, so use `action_remove_badge()` first if you need to avoid duplicates.
#'
#' @examples
#' \dontrun{
#' action_add_badge("ghclass-test/hw1-team01")
#'
#' action_remove_badge("ghclass-test/hw1-team01")
#' }
#'
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

  host = github_host_url()
  d[["url"]] =  glue::glue_data(d, "{host}/{repo}/workflows/{workflow}/badge.svg")
  d[["dest"]] = glue::glue_data(d, "{host}/{repo}/actions?query=workflow:\"{workflow}\"")
  d[["link"]] = glue::glue_data(d, "[![{workflow}]({url_encode(url)})]({url_encode(dest)})")

  # Collapse by repo to save multiple changes to a single file
  d = dplyr::group_by(d, repo) %>%
    dplyr::summarize(
      link = paste0(paste(.data$link, collapse = " "), line_padding),
      workflows = list(workflow)
    )

  res = status_scope(
    "Adding badges", nrow(d),
    done = "Added badges to {n_ok} of {total} repo{?s}",
    purrr::pmap(
      d,
      function(repo, link, workflows) {
        repo_txt = format_repo(repo, NULL, file)

        res = modify_file(
          repo = repo, path = file, pattern = where, content = link,
          method = "before", all = FALSE,
          message = cli::pluralize("Add {workflows} badge{?s} to {repo_txt}"),
          branch = NULL
        )

        status_msg(
          res,
          "Added {.val {workflows}} badge{?s} to {.val {repo_txt}}.",
          "Failed to add {.val {workflows}} badge{?s} to {.val {repo_txt}}."
        )
      }
    )
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
        "\\[!\\[{workflow_pat}\\]\\(.*?\\)\\]\\(https?://[^/]+/.*?/actions.*?\\)\\s*"
      )

      repo_txt = format_repo(repo, NULL, file)

      res = modify_file(
        repo = repo, path = file, pattern = pattern, content = "",
        method = "replace", all = TRUE,
        message = paste0("Remove workflow badges from ", repo_txt),
        branch = NULL
      )

      status_msg(
        res,
        "Removed workflow badges from {.val {repo_txt}}.",
        "Failed to remove workflow badges from {.val {repo_txt}}."
      )
    }
  )

  invisible(res)
}
