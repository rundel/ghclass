#' @rdname repo_core
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. One or more repository addresses in "owner/name" format.
#' @param overwrite Logical. Should the target repositories be overwritten. Default `FALSE`.
#' @param verbose Logical. Display verbose output. Default `FALSE`.
#' @param warn Logical. Warn the user about the function being deprecated. Default `TRUE`.
#'
#' @export
#'

repo_mirror = function(source_repo, target_repo, overwrite=FALSE, verbose=FALSE, warn=TRUE) {
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)
  arg_is_lgl_scalar(overwrite, verbose)

  target_repo = unique(target_repo)

  if (warn)
    .Deprecated("repo_mirror_template", package = "ghclass")


  withr::local_dir(tempdir())

  tmpdir = getwd()

  dir = file.path(tmpdir, get_repo_name(source_repo))
  unlink(dir, recursive = TRUE) # Make sure the source repo local folder does not exist

  repos = repo_n_commits(target_repo, quiet = TRUE) %>%
    dplyr::select("repo", "n")

  local_repo_clone(source_repo, tmpdir, mirror = TRUE, verbose = verbose)

  warned = FALSE

  res = status_scope(
    "Mirroring repos", nrow(repos),
    done = "Mirrored {.val {source_repo}} to {n_ok} of {total} repo{?s}",
    purrr::pmap(
      repos,
      function(repo, n) {
        repo_url = cli_glue("{github_host_url()}/{repo}.git")

        if (is.na(n)) {
          status_fail("The repo {.val {repo}} does not exist")
        } else if (n > 1 & !overwrite) {
          msg = paste(
            "The repo {.val {repo}} has more than one commit",
            "(n_commit = {.val {n}})."
          )

          if (!warned) {
            msg = paste(
              msg,
              "Use {.code overwrite = TRUE} if you want to permanently",
              "overwrite this repository."
            )
            warned <<- TRUE
          }

          status_fail(msg)
        } else {
          res = local_repo_push(dir, remote = repo_url, force = TRUE, prompt = FALSE, mirror = TRUE, verbose = verbose)
          status_msg(res[[1]])
          res
        }
      }
    )
  )

  unlink(dir, recursive = TRUE)
  cli::cli_alert_success("Removed local copy of {.val {source_repo}}")

  invisible(res)
}
