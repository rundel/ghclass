#' @title Local repository tools
#'
#' @description Returns up to `max` entries from a git repo's log
#'
#' @param repo_dir Vector of repo directories or a single directory containing one or more repos.
#' @param max Maximum number of log entries to retrieve per repo.
#'
#' @examples
#' \dontrun{
#' local_repo_log("hw1/")
#' }
#'

local_repo_log = function(repo_dir, max = 100) {
  require_gert()
  arg_is_chr(repo_dir)

  repo_dir = repo_dir_helper(repo_dir)

  purrr::map_dfr(
    repo_dir,
    function(dir) {
      log = gert::git_log(repo = dir, max = max)
      log = cbind(repo = dir, log)
      tibble::as_tibble(log)
    }
  )
}
