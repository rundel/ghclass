#' @rdname repo_core
#' @param status Logical. Should forking be allowed for the repository. Default `TRUE`.
#' @export
#'
repo_set_forking = function(repo, status = TRUE) {
  arg_is_chr(repo)
  arg_is_lgl(status)

  d = tibble::tibble(repo, status)

  res = status_scope(
    "Changing forking status", nrow(d),
    done = "Changed forking status for {n_ok} of {total} repo{?s}",
    purrr::pmap(
      d,
      function(repo, status) {
        res = purrr::safely(github_api_repo_edit)(repo, allow_forking = status)

        status_msg(
          res,
          "Changed the forking status of repo {.val {repo}} to {.val {status}}.",
          "Failed to change forking status of repo {.val {repo}}."
        )
      }
    )
  )

  invisible(res)
}
