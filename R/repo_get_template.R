#' Check if a GitHub repository is a template repository
#'
#' `repo_is_template` returns TRUE if the github repository is a template repository.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#'
#' @examples
#' \dontrun{
#' repo_is_template(c("rundel/ghclass", "rundel/ghclass_fake"))
#' }
#'
#' @return A logical vector
#'
#' @export
#'
repo_is_template = function(repo) {

  arg_is_chr(repo)

  # Checking if repo exists
  purrr::map_lgl(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo)(repo)
      status_msg(
        res,
        fail = glue::glue("Failed to retrieve repo {usethis::ui_value(repo)}.")
      )

      if (succeeded(res)) {
        result(res)[["is_template"]]
      } else {
        NA
      }
    }
  )
}
