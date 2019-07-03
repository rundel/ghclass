#' @rdname local_repo
#' @export
local_repo_clone = function(repo, local_path="./", branch = "master",
                      git = require_git(), options = character(),
                      absolute_path = TRUE, verbose = FALSE)
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

#' @rdname local_repo
#' @export
repo_clone = function(repo, local_path="./", branch = "master",
                      git = require_git(), options = character(),
                      absolute_path = TRUE, verbose = FALSE)
{
  local_repo_clone(repo, local_path, branch,
                   git, options, absolute_path, verbose)
}
