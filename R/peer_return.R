


#' Return peer feedback to authors
#'
#'#' `peer_assign` adds files from authors' repositories to review repositories. The function creates an issue on the reviewers' repositories informing them that the review files are available and creates links to the relevant documents.
#' `peer_return()` returns the review files from reviewers' review repositories to authors' repositories. The function i) adds empty rating form (if specified via `local_path_rating`), ii) moves completed review (if specified via `form_review`) from reviewer to author, iii) moves assignment files from reviewer to author (if specified via `path` and changed by reviewer), and iv) opens an issue on authors' repositories informing them about the added files.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param path Character. File name or vector of file names to be included. Cannot be left empty.
#' @param form_review Character. File name of reviewer feedback form (must be .Rmd document). If `NULL`, no review form will be moved back to authors' repositories.
#' @param local_path_rating Character. Local file path of rating feedback form to be added (must be .Rmd document), defaults to `NULL`. If `NULL`, no rating form will be added to authors' repositories.
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `TRUE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message, defaults to "Assigning review."
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @examples
#' \dontrun{
#' peer_return(org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' path = c("hw2_task.Rmd", "iris_data.csv"),
#' form_review = "hw2_review.Rmd",
#' local_path_rating = "./hw2/hw2_rating.Rmd",
#' prefix = "hw2-")
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_return = function(org, roster, path, form_review = NULL, local_path_rating = NULL, double_blind = TRUE,
                       prefix = "", suffix = "", message = NULL, branch = "master", overwrite = FALSE) {
  arg_is_chr(path)
  arg_is_chr_scalar(org, prefix, suffix, branch)
  arg_is_chr_scalar(message, form_review, local_path_rating, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  if (!double_blind) {
    rev = rdf[['rev']]
  } else {
    rev = rdf[['rev_no']]
  }

  # Contents of blank rating form from local file
  content_rating = local_path_content_grab(local_path_rating)

  # Take snapshot of reviewers' review repos
  repo_rev_og = unique(rdf[['repo_rev_review']])
  repo_files_rev_og = purrr::map(repo_rev_og, ~ repo_files(.x))

  # Take snapshot of authors' repos
  repo_aut_og = unique(rdf[['repo_aut']])
  repo_files_aut_og = purrr::map(repo_aut_og, ~ repo_files(.x))

  # get original content
  content_og = purrr::map(
    seq_along(repo_aut_og),
    function(a) {
      repo_path_content_grab(
        repo = repo_aut_og[a],
        path = path,
        repo_files = repo_files_aut_og[[a]],
        branch = branch
      )
    }
  )

  # The `purrr::map_df` statement below goes through multiple steps in the
  # return review process: add rating forms (if applicable), copy authors'
  # original content into reviewer-specific folders on authors' repositories
  # (to create a difference view on GitHub for the author), copy select
  # assignment files from reviewer to author repositores, copy completed review
  # forms from reviewer to author repositores, and create an issue for the author.
  # We keep all the steps in a single iteration over author repositories
  # in order to reduce calls to the GitHub API, which are limited to 5000
  # per hour.
  out = purrr::map_df(
    seq_len(nrow(rdf)),
    function(x) {
      repo_aut = rdf[['repo_aut']][x]
      n_a = which(repo_aut_og == repo_aut)
      repo_files_aut = repo_files_aut_og[[n_a]]

      repo_rev = rdf[['repo_rev_review']][x]
      n_r = which(repo_rev_og == repo_rev)
      repo_folder_rev = rdf[['aut_random']][x]
      repo_files_rev = repo_files_rev_og[[n_r]]

      # 1. Place rating form if applicable
      # What if no rating form
      # Check if the vectorized version still works
      if (length(content_rating) > 0) {
        rt = peer_add_content(
          target_repo = repo_aut,
          target_folder = rev[x],
          target_files = repo_files_aut,
          content = content_rating,
          category = "rating",
          message = "Adding rating form",
          branch = branch,
          overwrite = overwrite
        )

        # Keeping track of added files manually to reduce API calls
        # repo_files_aut_path used in peer_add_content below to check
        # whether file exists on author repository
        repo_files_aut_patch = unique(rbind(repo_files_aut,
                                            rt[names(rt) %in% names(repo_files_aut)]))
      } else {
        rt = NULL
        repo_files_aut_patch = repo_files_aut
      }

      # 2. place original content
      og = peer_add_content(
        target_repo = repo_aut,
        target_folder = rev[x],
        target_files = repo_files_aut_patch,
        content = content_og[[n_a]],
        category = "original",
        message = "Adding original file",
        branch = branch,
        overwrite = TRUE
      )

      # 3. Copy assignment files from reviewer
      # (difference will be created from this)
      ## i. Grab content from review repos & remove folder
      path_folder = glue::glue("{repo_folder_rev}/{path}")
      content_folder = content_path_folder_strip(
        repo_path_content_grab(
          repo = repo_rev,
          path = path_folder,
          repo_files = repo_files_rev,
          branch = branch
        ),
        repo_folder_rev
      )

      # Keeping track of files on author repositores to reduce API calls
      repo_files_aut_patch2 = unique(rbind(repo_files_aut_patch,
                                           og[names(og) %in% names(repo_files_aut_patch)]))

      ## ii. Place content
      mv = peer_add_content(
        target_repo = repo_aut,
        target_folder = rev[x],
        target_files = repo_files_aut_patch2,
        content = content_folder,
        content_compare = content_og[[n_a]],
        category = "assignment",
        message = "Adding reviewer feedback",
        branch = branch,
        overwrite = TRUE
      )

      # 4. Move reviewer form
      # i. Grab content
      if (!is.null(form_review)) {
        # Keeping track of files on author repositores to reduce API calls
        repo_files_aut_patch3 = unique(rbind(repo_files_aut_patch2,
                                             mv[names(mv) %in% names(repo_files_aut_patch2)]))

        path_review = glue::glue("{repo_folder_rev}/{form_review}")
        content_review = content_path_folder_strip(
          repo_path_content_grab(
            repo = repo_rev,
            path = path_review,
            repo_files = repo_files_rev,
            branch = branch
          ),
          repo_folder_rev
        )

        ## ii. Place content
        rv = peer_add_content(
          target_repo = repo_aut,
          target_folder = rev[x],
          target_files = repo_files_aut_patch3,
          content = content_review,
          category = "review",
          message = "Adding reviewer form",
          branch = branch,
          overwrite = overwrite
        )
      } else {
        rv = NULL
      }

      # 5. compile output
      rbind(rt, og, mv, rv)
    }
  )

  # 6. Create issue
  peer_issue_create(
    out = out,
    title = "Returning review",
    step = "rating",
    org = org,
    prefix = prefix,
    suffix = suffix,
    branch = branch
  )

}
