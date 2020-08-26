#' Add local files to reviewer-specific folders on authors' repositories
#'
#' `peer_file_add_aut()` takes a local file and adds it to reviewer-specific folders on authors' repositories. The function's main purpose is to distribute rating forms into the reviewer-specific folders on authors' repositories.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param local_path Character. File name of file to be added.
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the reviewer's ID. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `TRUE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#' @param verbose Logical. Should success/failure messages be printed, defaults to `TRUE`.
#'
#' @examples
#' \dontrun{
#' peer_file_add_aut(org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' local_path = "/Users/profx/introstats/hw2/hw2_rating.Rmd",
#' prefix = prefix)
#' }
#'
#' @family peer review functions
#'
#' @export
peer_file_add_aut = function(org, roster, local_path, double_blind = TRUE, prefix = "", suffix = "",
                             message = NULL, branch = "master", overwrite = FALSE, verbose = TRUE) {

  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr_scalar(message, allow_null = TRUE)
  arg_is_chr(local_path)
  arg_is_lgl(double_blind, overwrite, verbose)

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  file_status = fs::file_exists(local_path)
  if (any(!file_status))
    cli_stop("Unable to locate the following file(s): {.val {local_path[!file_status]}}")

  aut = unique(rdf[['repo_aut']])

  purrr::walk(
    aut,
    function(aut) {
      repo_files = repo_files(aut)
      if (!double_blind) {
        rev = rdf[['rev']][rdf[['repo_aut']] == aut]
      } else {
        rev = rdf[['rev_no']][rdf[['repo_aut']] == aut]
      }

      peer_file_place(
        repo_files = repo_files,
        target_repo = aut,
        input = purrr::cross2(rev, local_path),
        message = message,
        branch = branch,
        verbose = verbose,
        overwrite = overwrite
      )
    }
  )
}
