github_api_get_collaborators = function(repo) {
  gh::gh(
    "GET /repos/:owner/:repo/collaborators",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token(),
    .limit = get_github_api_limit()
  )
}

#' List collaborator(s)
#'
#' `get_collaborator` Returns a vector of collaborator user names. Users with admin rights are by default excluded.
#'
#' @param repo Character. Address of one or more repositories in `owner/name` format.
#' @param include_admin Logical. If `FALSE`, user names of users with Admin rights are not included, defaults to `TRUE`.
#'
#' @return A tibble with two columns, `repo` and `username`.
#'
#' @examples
#' \dontrun{
#' get_collaborators("ghclass-test/test2")
#' }
#'
#' @export
#'
get_collaborator = function(repo, include_admin = TRUE) {

  arg_is_chr(repo)

  org = unique(get_repo_owner(repo))
  stopifnot(length(org) == 1)

  admin = character()
  if (!include_admin)
    admin = get_admin(org)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_get_collaborators)(repo)
      status_msg(
        res,
        fail = glue::glue("Failed to retrieve collaborators for {usethis::ui_value(repo)}.")
      )

      collabs = result(res)

      d = if (empty_result(collabs)) {
        tibble::tibble(
          repo = character(),
          username = character()
        )
      } else {
        tibble::tibble(
          repo = repo,
          username = purrr::map_chr(collabs, "login")
        )
      }

      d[!d[["username"]] %in% admin, ]
    }
  )
}
