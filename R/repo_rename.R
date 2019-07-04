github_api_rename_repo = function(repo, new_name){
  gh::gh(
    "PATCH /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    name = new_name,
    .token = github_get_token()
  )
}

#' Rename repository
#'
#' `rename_repo` renames repositories. Use with caution as repositories retain their
#' unique identifier upon renaming and can be accessed under their old names due to
#' GitHub re-directing.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param new_name Character. New name of repository in the "name" format.
#'
#' @examples
#' \dontrun{
#' rename_repo("ghclass-test/hw1", "homework1")
#' }
#'
#' @export
#'
rename_repo = function(repo, new_name) {
  arg_is_chr(repo, new_name)

  purrr::walk2(
    repo, new_name,
    function(repo, new_name) {
      status_msg(
        purrr::safely(github_api_rename_repo)(repo, new_name),
        glue::glue("Renamed repo {usethis::ui_value(repo)} to {usethis::ui_value(new_name)}."),
        glue::glue("Failed to rename repo {usethis::ui_value(repo)} to {usethis::ui_value(new_name)}.")
      )
    }
  )
}r
