#' Ignore repository
#'
#' Ignores the provided GitHub repositories.
#'
#' @param repo github repository address in `owner/repo` format
#'

#' @aliases ignore_repo
#'
#' @export
#'
repo_ignore = function(repo) {
  arg_is_chr(repo)

  purrr::walk(
    repo,
    function(repo, notifications) {
      res = purrr::safely(github_api_repo_subscribe)(
        repo,
        subscribed = FALSE,
        ignored = TRUE
      )

      status_msg(
        res,
        glue::glue("Ignored {usethis::ui_value(repo)}."),
        glue::glue("Failed to ignore {usethis::ui_value(repo)}.")
      )
    }
  )
}
