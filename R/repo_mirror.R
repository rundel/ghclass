#' Mirror repository
#'
#' `repo_mirror` mirrors the content of one repository to another repository, or set of
#' repositories.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. One or more repository addresses in "owner/name" format.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' repo_mirror("ghclass-test/hw1", c("ghclass-test/hw1-Team1", "ghclass-test/hw1-Team2"))
#' repo_mirror("ghclass-test/hw1", org_repos("ghclass-test","hw1-"))
#' }
#'
#' @aliases mirror_repo
#'
#' @export
#'
repo_mirror = function(source_repo, target_repo, verbose=FALSE) {
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)

  withr::local_dir(tempdir())

  dir = local_repo_clone(source_repo, getwd(), options = "--bare", verbose = verbose)

  purrr::walk(
    target_repo,
    function(repo) {
      local_repo_mirror_push(dir, repo, verbose = verbose)
    }
  )

  unlink(file.path(dir), recursive = TRUE)
  usethis::ui_done("Removed local copy of {usethis::ui_value(source_repo)}")
}
