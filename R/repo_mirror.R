#' Mirror repository
#'
#' `repo_mirror` mirrors the content of one repository to another repository, or set of
#' repositories.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. One or more repository addresses in "owner/name" format.
#' @param overwrite Logical. Should the target repositories be overwritten.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' repo_mirror("ghclass-test/hw1", c("ghclass-test/hw1-Team1", "ghclass-test/hw1-Team2"))
#' repo_mirror("ghclass-test/hw1", org_repos("ghclass-test","hw1-"))
#' }
#'
#' @export
#'

repo_mirror = function(source_repo, target_repo, overwrite=FALSE, verbose=FALSE) {
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)
  arg_is_lgl_scalar(overwrite, verbose)

  withr::local_dir(tempdir())
  dir = file.path(getwd(), get_repo_name(source_repo))
  unlink(dir, recursive = TRUE) # Make sure the source repo local folder does not exist

  repos = repo_n_commits(target_repo, quiet = TRUE)

  local_repo_clone(source_repo, getwd(), mirror = TRUE, verbose = verbose)

  warned = FALSE

  purrr::pwalk(
    repos,
    function(repo, n) {
      repo_url = glue::glue("https://github.com/{repo}.git")

      if (is.na(n)) {
        usethis::ui_oops("The repo {usethis::ui_value(repo)} does not exist")
      } else if (n > 1 & !overwrite) {
        msg = paste(
          "The repo {usethis::ui_value(repo)} has more than one commit",
          "(n_commit = {usethis::ui_value(n)})."
        )

        if (!warned) {
          msg = c(msg, paste(
            "Use {usethis::ui_code('overwrite = TRUE')} if you want to permanently",
            "overwrite this repository."
          ))
          warned <<- TRUE
        }

        usethis::ui_oops( msg )
      } else {
        local_repo_push(dir, remote = repo_url, force = TRUE, prompt = FALSE, mirror = TRUE, verbose = verbose)
      }
    }
  )

  unlink(dir, recursive = TRUE)
  usethis::ui_done("Removed local copy of {usethis::ui_value(source_repo)}")
}
