#' @title Local repository tools
#'
#' @description Clones repositories from GitHub to a local directory.
#'
#' @param repo GitHub repo names with the form `owner/name`.
#' @param local_path Local directory to store cloned repos.
#' @param branch Repository branch to use.
#' @param git Path to the local git binary. `require_git()` attempts to
#' find the git binary based on your `PATH``, it will throw an error if git cannot be found.
#' @param options Additional git binary options (e.g. `--all`).
#' @param verbose Display verbose output.
#'
#' @aliases repo_clone
#'
#' @examples
#' \dontrun{
#' g = org_repos("Sta323-Sp18","hw3-")
#' local_repo_clone(g, "hw3")
#' }
#'

#' @export
local_repo_clone = function(repo, local_path="./", branch = "master",
                      git = require_git(), options = character(),
                      verbose = FALSE)
{
  stopifnot(!missing(repo))
  stopifnot(file.exists(git))

  local_path = fs::path_expand(local_path)

  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  dirs = purrr::map2_chr(
    repo, branch,
    function(repo, branch) {
      dir = fs::path(local_path, get_repo_name(repo))

      if (!branch %in% c("", "master"))
        options = c("-b", branch, options)

      res = purrr::safely(run_git)(
        git, "clone", c(options, get_repo_url(repo), dir), verbose = verbose
      )

      fmt_repo = format_repo(repo, branch)

      status_msg(
        res,
        glue::glue("Cloned {usethis::ui_value(fmt_repo)}."),
        glue::glue("Failed to clone {usethis::ui_value(fmt_repo)}.")
      )

      dir
    }
  )

  invisible(dirs)
}

#' @export
repo_clone = function(...)
{
  local_repo_clone(...)
}
