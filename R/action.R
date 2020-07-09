#' @name action
#' @rdname action
#'
#' @title GitHub Actions tools
#'
#' @description
#' `action_workflows` - returns a data frame containing details on a repositories workflows.
#'
#' `action_add_badge` - Add a GitHub Actions badge to a file.
#'
#' `action_remove_badge` - Remove one or more GitHub Action badges from a file.
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param full Logical. Should all workflow columns be returned
#' @param where Character. Regex pattern of where to insert the badge, defaults
#' to the beginning of the README
#' @param line_padding Character. What should be added to the end of the link,
#' defaults to sufficient new lines to add a blank line after the badge.
#' @param file Character. Target file to be modified, defaults to `README.md`.#'
#'
#' @examples
#' \dontrun{
#' action_workflows("rundel/ghclass")
#' }
#'
NULL

