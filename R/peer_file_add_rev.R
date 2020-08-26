#' Add local files to author-specific folders on reviewers' review repositories
#'
#' `peer_file_add_rev()` takes a local file and adds it to author-specific folders on reviewers' repositories. The function's main purpose is to distribute review forms into the correct author-specific folders on reviewers' repositories.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param local_path Character. File name of file to be added.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#' @param verbose Logical. Should success/failure messages be printed, defaults to `TRUE`.
#'
#' @examples
#' \dontrun{
#' peer_file_add_rev(org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' local_path = "/Users/profx/introstats/hw2/hw2_review.Rmd",
#' prefix = prefix)
#' }
#'
#' @family peer review functions
#'
#' @export
peer_file_add_rev = function(org, roster, local_path, prefix = "", suffix = "",
                             message = NULL, branch = "master", overwrite = FALSE,
                             verbose = TRUE) {
  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr_scalar(message, branch, allow_null = TRUE)
  arg_is_chr(local_path)
  arg_is_lgl(overwrite, verbose)

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  file_status = fs::file_exists(local_path)
  if (any(!file_status))
    cli_stop("Unable to locate the following file(s): {.val {local_path[!file_status]}}")

  rev = unique(rdf[['repo_rev_review']])

  purrr::walk(
    rev,
    function(rev) {
      repo_files = repo_files(rev, branch = branch)
      aut = rdf[['aut_random']][rdf[['repo_rev_review']] == rev]

      peer_file_place(
        repo_files = repo_files,
        target_repo = rev,
        input = purrr::cross2(aut, local_path),
        message = message,
        branch = branch,
        verbose = verbose,
        overwrite = overwrite
      )
    }
  )
}
