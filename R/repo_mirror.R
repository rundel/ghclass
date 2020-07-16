#' @rdname repo_core
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. One or more repository addresses in "owner/name" format.
#' @param overwrite Logical. Should the target repositories be overwritten.
#' @param verbose Logical. Display verbose output.
#'
#' @export
#'

repo_mirror = function(source_repo, target_repo, overwrite=FALSE, verbose=FALSE) {
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)
  arg_is_lgl_scalar(overwrite, verbose)

  .Deprecated("repo_mirror_template", package = "ghclass")

  withr::local_dir(tempdir())
  dir = file.path(getwd(), get_repo_name(source_repo))
  unlink(dir, recursive = TRUE) # Make sure the source repo local folder does not exist

  repos = repo_n_commits(target_repo, quiet = TRUE) %>%
    dplyr::select(.data$repo, .data$n)

  local_repo_clone(source_repo, getwd(), mirror = TRUE, verbose = verbose)

  warned = FALSE

  purrr::pwalk(
    repos,
    function(repo, n) {
      repo_url = cli_glue("https://github.com/{repo}.git")

      if (is.na(n)) {
        cli::cli_alert_danger("The repo {.val {repo}} does not exist")
      } else if (n > 1 & !overwrite) {
        msg = paste(
          "The repo {.val {repo}} has more than one commit",
          "(n_commit = {.val {n}})."
        )

        if (!warned) {
          msg = c(msg, paste(
            "Use {.code overwrite = TRUE} if you want to permanently",
            "overwrite this repository."
          ))
          warned <<- TRUE
        }

        cli::cli_alert_danger( msg )
      } else {
        local_repo_push(dir, remote = repo_url, force = TRUE, prompt = FALSE, mirror = TRUE, verbose = verbose)
      }
    }
  )

  unlink(dir, recursive = TRUE)
  cli::cli_alert_success("Removed local copy of {.val {source_repo}}")
}
