#' @name action
#' @rdname action
#'
#' @title Retrieve information about GitHub Actions workflows and their runs.
#'
#' @description
#' * `action_workflows()` - retrieve details on repo workflows.
#'
#' * `action_runs()` - retrieve details on repo workflow runs.
#'
#' * `action_status()` - DEPRECATED - retrieve details on most recent workflow runs.
#'
#' * `action_runtime()` - retrieves runtime durations for workflow runs.
#'
#' * `action_artifacts()` - retrieve details on available workflow artifacts.
#'
#' * `action_artifact_download()` - downloads artifact(s) into a local directory.
#'
#' * `action_artifact_delete()` - deletes artifact(s).
#'
#' @param repo Character. Address of repositories in `owner/name` format.
#' @param dir Character. Path to the directory where artifacts will be saved.
#' @param ids Integer or data frame. Artifact ids to be downloaded or deleted.
#' If a data frame is passed then the `id` column will be used.
#'
#' @return
#'
#' `action_workflows()`, `action_runs()`, `action_runtime()`, and `action_artifacts`
#' all return tibbles containing information on requested repos' available workflows,
#' recent workflow runs, workflow runs runtimes, and generated artifacts
#' respectively.
#'
#' `action_artifact_download()` returns a character vector containing the paths of all
#' downloaded fules
#'
#' `action_artifact_delete()` returns an invisible data frame containing repository names and
#'  ids of the deleted artifacts.
#'
#' @examples
#' \dontrun{
#' action_workflows("rundel/ghclass")
#'
#' action_runs("rundel/ghclass")
#'
#' action_runtime(c("rundel/ghclass", "rundel/parsermd"))
#'
#' action_artifacts(c("rundel/ghclass", "rundel/parsermd"))
#' }
#'
NULL

#' @name action_badge
#' @rdname action_badge
#'
#' @title Add or remove GitHub Actions badges from a repository
#'
#' @description
#' * `action_add_badge()` - Add a GitHub Actions badge to a file.
#'
#' * `action_remove_badge()` - Remove one or more GitHub Action badges from a file.
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
#' @return Both `action_add_badge()` and `action_remove_badge()` invisibly return a list
#' containing the results of the relevant GitHub API call.
#'
NULL

