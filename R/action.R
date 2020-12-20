#' @name action
#' @rdname action
#'
#' @title Retrieve information about GitHub Actions workflows and their runs.
#'
#' @description
#' * `action_workflows` - retrieve details on repo workflows.
#'
#' * `action_runs` - retrieve details on repo workflow runs.
#'
#' * `action_status` - retrieve details on most recent workflow runs.
#'
#' @examples
#' \dontrun{
#' action_workflows("rundel/ghclass")
#'
#' action_runs("rundel/ghclass")
#'
#' action_status(c("rundel/ghclass", "rundel/parsermd"))
#' }
#'
NULL

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

