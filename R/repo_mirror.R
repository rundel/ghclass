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
#' @aliases mirror_repo
#'
#' @export
#'

repo_mirror = function(source_repo, target_repo, overwrite=FALSE, verbose=FALSE) {
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)
  arg_is_lgl_scalar(overwrite, verbose)

  withr::local_dir(tempdir())

  dir = file.path(getwd(), get_repo_name(source_repo))
  unlink(dir, recursive = TRUE)

  n_commits = table(repo_commits(target_repo)[["repo"]])

  missing_repos = target_repo[!target_repo %in% names(n_commits)]
  if (length(missing_repos) != 0) {
    usethis::ui_stop( paste(
      "The following {usethis::ui_code('target_repo')}s do not exist:",
      "{usethis::ui_value(missing_repos)}"
    ) )
  }

  if (!overwrite & any(n_commits > 1)) {
    usethis::ui_stop( c(
      "The following {usethis::ui_code('target_repo')}s have more than a single initialization commit:",
      "\t{usethis::ui_value(target_repo[n_commits > 1])}.",
      "This process will permanently overwrite these repositories. If you are sure this is what",
      "you want to do re-run this function with {usethis::ui_code('overwrite = TRUE')}"
    ) )
  }

  local_repo_clone(source_repo, getwd(), mirror = TRUE, verbose = verbose)

  purrr::walk(
    target_repo,
    function(repo) {
      repo_url = glue::glue("https://github.com/{repo}.git")


      local_repo_push(dir, remote = repo_url, force = TRUE, prompt = FALSE, mirror = TRUE, verbose = verbose)
    }
  )

  unlink(dir, recursive = TRUE)
  usethis::ui_done("Removed local copy of {usethis::ui_value(source_repo)}")
}
