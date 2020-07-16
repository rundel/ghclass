#' @rdname local_repo
#' @param max Maximum number of log entries to retrieve per repo.
#' @export
#'
local_repo_log = function(repo_dir, max = 100) {
  require_gert()
  arg_is_chr(repo_dir)

  repo_dir = repo_dir_helper(repo_dir)

  purrr::map_dfr(
    repo_dir,
    function(dir) {
      gert::git_log(repo = dir, max = max) %>%
        dplyr::mutate(repo = dir) %>%
        dplyr::relocate(.data$repo)
    }
  )
}
