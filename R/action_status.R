#' @name action
#' @rdname action
#'
#' @title Return a data frame with the status of the most recent workflow runs for each repo.
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param branch Character. Vector of branches to report.
#' @param before Datetime. Filter all runs created after this timestamp.
#' Character values are converted using `lubridate::ymd_hms()`.
#' @param limit Numeric. Maximum number of workflow runs to return. If runs are
#' missing try increasing this limit, if performance is slow try decreasing it.
#'
#' @examples
#' \dontrun{
#' action_status(c("rundel/ghclass", "rundel/parsermd"))
#' }
#'
#' @export
#'
action_status = function(repo, branch = NULL, before = NULL, limit = 50) {
  repo = unique(repo)
  arg_is_chr(repo)
  arg_is_chr(branch, allow_null = TRUE)
  arg_is_pos_int_scalar(limit)

  if (is.character(before))
    before = lubridate::ymd_hms(before)
  stopifnot(is.null(before) | lubridate::is.POSIXct(before))



  res = purrr::map_dfr(
    repo,
    ~ {
      action_runs(.x, limit = limit) %>%
        dplyr::mutate(repo = .x) %>%
        dplyr::relocate(repo)
    }
  )

  if (!is.null(branch)) {
    br = branch
    res = dplyr::filter(res, branch %in% br | is.na(branch))
  }

  to_report = res %>%
    dplyr::select(repo, name, branch) %>%
    dplyr::distinct()

  res = res %>%
    dplyr::group_by(repo, name, branch) %>%
    dplyr::arrange(dplyr::desc(created)) %>%
    dplyr::slice(1) %>%
    dplyr::select(repo, name, branch, result, created)

  if (!is.null(before))
    res = dplyr::filter(res, created < before)

  dplyr::full_join(
    to_report, res, by = c("repo", "name", "branch")
  )
}
