#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to review repositories. The function creates an issue on the reviewers' repositories informing them that the review files are available and creates links to the relevant documents.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param path Character. File name or vector of file names to be included. If `NULL`, all files not contained in folders, except `.gitignore`, `.Rhistory`, `.Rproj`, `*.html`, `*.md`, and `*.pdf` will be moved to the reviewers' repositories.
#' @param local_path_review Character. Local file path of review feedback form to be added (must be .Rmd document), defaults to `NULL`. If `NULL`, no review form will be added to authors' repositories.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param exclude_pattern Character. File extensions of files to not be moved to reviewer repositories if `path` is `NULL`, defaults to `c(".gitignore", ".Rhistory", "*.Rproj", "*.html", "*.md", "*.pdf")`.
#' @param message Character. Commit message, defaults to "Assigning review."
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @examples
#' \dontrun{
#' peer_assign(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' path = c("hw2_task.Rmd", "iris_data.csv"),
#' form_review = "hw2_review.Rmd",
#' prefix = "hw2-"
#' )
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_assign = function(org, roster, path = NULL, local_path_review = NULL,
                       prefix = "", suffix = "",
                       exclude_pattern = c(".gitignore", ".Rhistory", "*.Rproj", "*.html", "*.md", "*.pdf"),
                       message = NULL,  branch = "master", overwrite = FALSE) {

  arg_is_chr_scalar(org, prefix, suffix, branch)
  arg_is_chr_scalar(local_path_review, message, allow_null = TRUE)
  arg_is_chr(path, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  # Contents of blank review form from local file
  content_review = local_path_content_grab(local_path = local_path_review,
                                           check_rmd = TRUE)

  # The `purrr::map_df` statement below goes through multiple steps in the
  # assignment process: i. add review forms (if applicable), determine which
  # files should be moved from author repositories (if no path is specified
  # by user), grab the content of the paths, and copy them to each of the
  # reviewers, and open an issue on reviewers' review repositores.
  # We keep all the steps in a single iteration over author repositories
  # in order to reduce calls to the GitHub API, which are limited to 5000
  # per hour.
  out = purrr::map_df(
    unique(rdf[['aut']]),
    function(aut) {
      sub = rdf[rdf[['aut']] == aut, ]
      repo_aut = unique(sub[['repo_aut']])
      repo_rev = unique(sub[['repo_rev_review']])
      repo_files_aut = repo_files(repo = repo_aut, branch = branch)

      repo_folder_rev = unique(sub[['aut_random']])
      repo_files_rev = repo_files(repo = repo_rev, branch = branch)

      # 1. Place rating form
      # iterates over target_repo, but not content
      if (length(content_review) > 0) {
        rv = peer_add_content(
          target_repo = repo_rev,
          target_folder = repo_folder_rev,
          target_files = repo_files_rev,
          content = content_review,
          category = "review",
          message = "Adding review form",
          branch = branch,
          overwrite = overwrite
        )

        # Keeping track of added files manually to reduce API calls
        # repo_files_rev_patch is used in peer_add_content below to check whether
        # whether file exists on reviewer repository
        repo_files_rev_patch = unique(rbind(repo_files_rev,
                                            rv[names(rv) %in% names(repo_files_rev)]))

      } else {
        repo_files_rev = repo_files_rev
      }

      # 2. Copy files to reviewers
      ## i.  Select paths
      if (is.null(path)) {
        path = repo_files_select(
          repo = repo_aut,
          repo_files = repo_files_aut,
          exclude_pattern = exclude_pattern,
          branch = branch
        )
      }

      ## ii. Grab content
      content_repo = repo_path_content_grab(
        repo = repo_aut,
        path = path,
        repo_files = repo_files_aut,
        branch = branch
      )

      ## iii. Place content
      mr = peer_add_content(
        target_repo = repo_rev,
        target_folder = repo_folder_rev,
        target_files = repo_files_rev_patch,
        content = content_repo,
        category = "assignment",
        message = "Adding assignment files",
        branch = branch,
        overwrite = overwrite
      )

      rbind(rv, mr)
    }
  )

  # 3. Create issue
  peer_issue_create(
    out = out,
    title = "Assigning review",
    step = "review",
    org = org,
    prefix = prefix,
    suffix = suffix,
    branch = branch
  )
}
