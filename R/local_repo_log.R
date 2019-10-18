#' @rdname local_repo
#' @export
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
