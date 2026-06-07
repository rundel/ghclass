#' @rdname local_repo
#'
#' @param repo GitHub repo address with the form `owner/name`.
#' @param local_path Local directory to store cloned repos.
#'
#' @aliases repo_clone
#'
#' @export
#'
local_repo_clone = function(repo, local_path=".", branch = NULL, mirror = FALSE, verbose = FALSE) {
  require_gert()

  arg_is_chr(repo)
  arg_is_chr(branch, allow_null=TRUE)
  arg_is_chr_scalar(local_path)
  arg_is_lgl_scalar(mirror, verbose)

  if (is.null(branch) || mirror) # If mirroring then branch should not be set
    branch = list(NULL)

  local_path = fs::path_expand(local_path)
  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)

  dirs = purrr::map2_chr(
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
        "Cloned {.val {fmt_repo}}.",
        "Failed to clone {.val {fmt_repo}}."
      )

      ternary(succeeded(res), as.character(dir), NA_character_)
    }
  )

  names(dirs) = repo

  invisible(dirs)
}

#' @export
repo_clone = function(repo, local_path = ".", branch = NULL, mirror = FALSE, verbose = FALSE) {
  local_repo_clone(repo = repo, local_path = local_path, branch = branch, mirror = mirror, verbose = verbose)
}
