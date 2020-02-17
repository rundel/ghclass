#' @title Local repository tools
#'
#' @description Clones repositories from GitHub to a local directory.
#'
#' @param repo GitHub repo address with the form `owner/name`.
#' @param local_path Local directory to store cloned repos.
#' @param branch Repository branch to use.
#' @param mirror Use equivalent of `--mirror` when cloning.
#' @param verbose Display verbose output.
#'
#' @examples
#' \dontrun{
#' g = org_repos("Sta323-Sp18","hw3-")
#' local_repo_clone(g, "hw3")
#' }
#'

#' @export
local_repo_clone = function(repo, local_path=".", branch = "master", mirror = FALSE, verbose = FALSE) {
  require_gert()

  arg_is_chr(repo, branch)
  arg_is_chr_scalar(local_path)
  arg_is_lgl_scalar(mirror, verbose)

  if (mirror) # If mirroring then branch should not be set
    branch = list(NULL)

  local_path = fs::path_expand(local_path)
  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  res = purrr::map2(
    repo, branch,
    function(repo, branch) {
      dir = fs::path(local_path, get_repo_name(repo))
      url = glue::glue("https://github.com/{repo}.git")

      res = purrr::safely(gert::git_clone)(
        url = url, path = dir, branch = branch, mirror = mirror, verbose = verbose
      )

      fmt_repo = format_repo(repo, branch)

      status_msg(
        res,
        glue::glue("Cloned {usethis::ui_value(fmt_repo)}."),
        glue::glue("Failed to clone {usethis::ui_value(fmt_repo)}.")
      )

      res
    }
  )

  invisible(res)
}

#' @export
repo_clone = function(repo, local_path="./", branch = "master", verbose = FALSE) {
  local_repo_clone(repo=repo, local_path=local_path, branch=branch, verbose=verbose)
}
