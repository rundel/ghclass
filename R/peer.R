##---------------------------------------------------------
# Naming principles
#
# rev denotes reviewer
# aut renotes author
# _review indicates repositories that are specific for review files
#
##---------------------------------------------------------


#' Create peer review roster
#'
#' `peer_roster_create` creates data frame of random assignments of author files to reviewers. By default, the output is saved to a `.csv` file in the current working directory that incorporates the current date and random seed as part of the file name.
#'
#' @param user Character. A vector of GitHub user names.
#' @param n_rev Numeric. Number of reviews per user. Must be larger than zero and smaller than the number of users.
#' @param seed Numeric. Random seed for assignment, defaults to `12345`.
#' @param write_csv Logical. Whether the roster data frame should be written to a `.csv` file in the current working directory, defaults to TRUE.
#' @param dir Character. Directory where the peer review roster will be written if `write_csv = TRUE`.
#'
#' @examples
#' \dontrun{
#' peer_roster_create(3,
#' c("anya-ghclass", "bruno-ghclass", "celine-ghclass", "diego-ghclass"),
#' dir = "/Users/profx/introstats/hw2/")
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_roster_create = function(n_rev,
                              user,
                              seed = NULL,
                              write_csv = TRUE,
                              dir = NULL) {
  arg_is_chr(user)
  arg_is_chr(dir, allow_null = TRUE)
  arg_is_pos_int(n_rev)
  arg_is_pos_int_scalar(seed, allow_null = TRUE) #fails now

  if (!(length(user) > 1)) {
    usethis::ui_stop("{usethis::ui_field('user')} must contain more than one user name.")
  }
  if (!(n_rev < length(user))) {
    usethis::ui_stop(
      "{usethis::ui_field('n_rev')} must be smaller than the number of users in {usethis::ui_field('user')}."
    )
  }
  if (write_csv) {
    if (is.null(dir)) {
      usethis::ui_stop("No directory specified in {usethis::ui_field('dir')}.")
    } else if (!dir.exists(dir)) {
      usethis::ui_stop("Directory {usethis::ui_value(dir)} does not exist.")
    }
  }

  if (is.null(seed)) {
    seed = sample.int(1e+06, 1L)
    usethis::ui_warn("No seed was specified Using randomly sampled seed {usethis::ui_value(seed)}")
  }

  withr::with_seed(seed, {
    j = sample(2:length(user), n_rev)

    # Randomizing user names to avoid clustering
    user_random = paste0("aut", sample(1:length(user), length(user)))
  })

  res = purrr::map(j, ~ latin_square(.x, length(user)))


  df_sort = tibble::tibble(user = user,
                           user_random = user_random)[order(as.numeric(sub("[aA-zZ]+", "", user_random))),]

  df_tmp = purrr::set_names(if (length(user) > 2) {
    out = purrr::map_dfc(res, ~ df_sort[['user_random']][.x])
  } else {
    # if length(user) == 2, will always res = c(2, 1)
    out = tibble::tibble(rev(user_random))
  },
  paste0("rev", 1:n_rev))

  res_df = tibble::as_tibble(cbind(df_sort, df_tmp))
  attr(res_df, "seed") <- seed
  attr(res_df, "username") <- user
  attributes(res_df)

  if (write_csv) {
    fname = glue::glue("roster_seed{seed}.csv")
    readr::write_csv(res_df, glue::glue("{dir}/{fname}"))
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to {usethis::ui_value(dir)}.")
  }
  res_df
}

#' Initiate peer review repositories
#'
#' `peer_init()` initiates peer review repositories. It creates a review repository for each user, adds users to their respective repositories, and applies peer review labels to all repositories (i.e. assignment and review repositories).
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param verbose Logical. Whether information about label creation should be given, defaults to `FALSE`. Issues can still be created even labels were not applied to a particular repository.
#' @param show_label_result Logical. Whether a tibble with details on the creation of labels should be returned, defaults to `FALSE`.
#'
#' @examples
#' \dontrun{
#' peer_init(org = "ghclass-test", roster = "roster_test", prefix = "hw2-")
#' }
#'
#' @family peer review functions
#'
#' @export
peer_init = function(org,
                     roster,
                     prefix = "",
                     suffix = "",
                     verbose = FALSE,
                     show_label_result = FALSE) {
  arg_is_chr_scalar(org, prefix, suffix)

  prefix_review = format_rev(prefix, suffix)[["prefix_review"]]
  suffix_review = format_rev(prefix, suffix)[["suffix_review"]]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)
  user = unique(rdf[['rev']])

  repo_create(
    org = org,
    name = user,
    prefix = prefix_review,
    suffix = suffix_review
  )
  repo_add_user(glue::glue("{org}/{prefix_review}{user}{suffix_review}"),
                user)

  peer_issue_label_apply(
    org = org,
    repo = unique(c(rdf[['repo_aut']], rdf[['repo_rev_review']])),
    verbose = verbose,
    show_label_result = show_label_result
  )

}

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
peer_assign = function(org,
                       roster,
                       path = NULL,
                       local_path_review = NULL,
                       prefix = "",
                       suffix = "",
                       exclude_pattern = c(".gitignore", ".Rhistory", "*.Rproj", "*.html", "*.md", "*.pdf"),
                       message = NULL,
                       branch = "master",
                       overwrite = FALSE) {
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
  out = purrr::map_df(unique(rdf[['aut']]),
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
                      })

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


#' Create reviewer feedback form
#'
#' `peer_form_create_review` creates blank feedback forms for reviewers based on the user-specified number of questions.
#'
#' @param n Numerical. Number of score fields to be included in the YAML of the .Rmd file.
#' @param title Character. Title of form, defaults to "Reviewer feedback form."
#' @param fname Character. File name of RMarkdown document to be written to memory, defaults to `feedback_blank_review`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to `TRUE`.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param double_blind Logical. If `double_blind = TRUE`, the YAML will contain an `author` field, defaults to `TRUE`.
#'
#' @examples
#' \dontrun{
#' peer_form_create_review(5, "Reviewer feedback for HW2", "rfeedback_hw2_blank")
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_form_create_review = function(n,
                                   title = "Reviewer feedback form",
                                   fname = "feedback_blank_review",
                                   output = "github_document",
                                   write_rmd = TRUE,
                                   overwrite = FALSE,
                                   double_blind = TRUE) {
  stopifnot(!is.null(fname))
  if (grepl("\\s+", fname)) {
    fname = gsub("\\s", "_", fname)
  }
  if (grepl("\\.Rmd$", fname)) {
    fname = gsub("\\.Rmd$", "", fname)
  }

  # YAML
  yaml_txt = sprintf(if (!double_blind) {
    "---\ntitle: \"%s\"\nauthor: [INSERT NAME]\noutput: %s\nparams:\n%s\n---\n\n\n"
  } else {
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n"
  },
  title,
  output,
  paste(purrr::map_chr(1:n, function(x) {
    paste0("  q", x, "_score: [INSERT SCORE]")
  }),
  collapse = "\n"))


  # Body
  resp = "Your response goes here..."
  body_txt = paste(
    "## Instructions",
    "Enter your feedback for each question below. Please replace `[INSERT SCORE]` in the `q*_score` fields in the YAML with the scores you give the author for each question.",
    "Please keep your review comments constructive and professional.",
    "## Feedback",
    paste0(purrr::map(
      1:n,
      ~ paste0(
        sprintf(
          "#### %1$i. Place Question %1$i text here. [max. xxx points]\n\n",
          .x
        ),
        resp,
        collapse = "\n\n"
      )
    ),
    collapse = "\n\n"),
    sep = "\n\n"
  )

  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")

  if (write_rmd) {
    fname = paste0(fname, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, file = fname)
      usethis::ui_done("Saved file {usethis::ui_value(fname)}")
    } else {
      usethis::ui_oops(
        paste(
          'File {usethis::ui_value(fname)} already exists.',
          'If you want to force save this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
        )
      )
    }
  } else {
    doc_txt
  }
}


#' Create author rating form
#'
#' `peer_form_create_rating` creates a short feedback form for authors to rate the feedback they got from reviewers. The rating categories are based on Reily, K. and P. Ludford Finnerty,  and L. Terveen (2009): Two Peers Are Better Than One: Aggregating Peer Reviews for Computing Assignments is Surprisingly Accurate. In *Proceedings of the ACM 2009 International Conference on Supporting Group Work*. GROUP’09, May 10–13, 2009, Sanibel Island, Florida, USA.
#'
#' @param category Character. Categories to be included in the feedback form, defaults to `c("helpfulness", "accuracy", "fairness")`.
#' @param title Character. Title of form, defaults to "Author rating form".
#' @param fname Character. File name of RMarkdown document to be written to memory, defaults to `feedback_blank_rating`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param allow_comment Logical. Should optional comment field be included? Defaults to `FALSE`.
#'
#' @examples
#' \dontrun{
#' peer_form_create_rating(c("accuracy", "fairness"))
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_form_create_rating = function(category = c("helpfulness", "accuracy", "fairness"),
                                   title = "Author rating form",
                                   fname = "feedback_blank_rating",
                                   output = "github_document",
                                   write_rmd = TRUE,
                                   overwrite = FALSE,
                                   allow_comment = FALSE) {
  arg_is_chr(category)
  stopifnot(all(category %in% c("helpfulness", "accuracy", "fairness")))

  stopifnot(!is.null(fname))
  if (grepl("\\s+", fname)) {
    fname = gsub("\\s", "_", fname)
  }
  if (grepl("\\.Rmd$", fname)) {
    fname = gsub("\\.Rmd$", "", fname)
  }

  # YAML
  yaml_txt = sprintf(
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n",
    title,
    output,
    paste(paste0("  ", category, ": [INSERT SCORE]"), collapse = "\n")
  )

  # Instructions
  instruct_txt = sprintf(
    "Please rate the reviewer's feedback based on the categories below on a scale from \"Strongly disagree\" to \"Strongly agree.\" Please replace `[INSERT SCORE]` in the %s %s in the YAML with the scores you give the reviewer for each category.",
    if (length(category) == 1) {
      paste0("`", category, "`")
    } else if (length(category) == 2) {
      paste0(paste0("`", category[1], "`"),
             " and ",
             paste0("`", category[2], "`"))
    } else {
      paste0(paste0(purrr::map_chr(category[1:(length(category) - 1)], ~ paste0("`", .x, "`"))
                    , collapse = ", "),
             ", and ",
             paste0("`", category[length(category)], "`"))
    },
    if (length(category) == 1) {
      "field"
    } else {
      "fields"
    }
  )

  category_txt = list(helpfulness = "`helpfulness`: \"The reviewer's feedback was constructive and helpful.\"  [max. 4 points]",
                      accuracy = "`accuracy`: \"The reviewer's assessment accurately describes the quality of my work.\" [max. 4 points]",
                      fairness = "`fairness`: \"The reviewer's assessment was fair.\" [max. 4 points]")

  tab_txt = paste(
    "| Score | Rating            |",
    "|-------|-------------------|",
    "| 1     | Strongly disagree |",
    "| 2     | Disagree          |",
    "| 3     | Agree             |",
    "| 4     | Strongly agree    |",
    sep = "\n"
  )

  if (allow_comment) {
    comment_txt = "\n---\nIf you have additional comments for the reviewer, please enter it below. Please keep your review comments constructive and professional.\n\n[Your comments go here]"
  } else {
    comment_txt = NULL
  }

  # Putting it all together
  body_txt = paste(
    "## Instructions",
    instruct_txt,
    paste(paste0(
      1:length(category),
      ". ",
      purrr::map_chr(category, ~ paste(category_txt[which(names(category_txt) == .x)]))
    ),
    collapse = "\n\n"),
    tab_txt,
    comment_txt,
    sep = "\n\n"
  )


  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")

  if (write_rmd) {
    fname = paste0(fname, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, file = fname)
      usethis::ui_done("Saved file {usethis::ui_value(fname)}")
    } else {
      usethis::ui_oops(
        paste(
          'File {usethis::ui_value(fname)} already exists.',
          'If you want to force save this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
        )
      )
    }
  } else {
    doc_txt
  }
}



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
peer_file_add_rev = function(org,
                             roster,
                             local_path,
                             prefix = "",
                             suffix = "",
                             message = NULL,
                             branch = "master",
                             overwrite = FALSE,
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
    usethis::ui_stop(
      "Unable to locate the following file(s): {usethis::ui_value(local_path[!file_status])}"
    )

  rev = unique(rdf[['repo_rev_review']])

  purrr::walk(rev,
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
              })
}


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
peer_file_add_aut = function(org,
                             roster,
                             local_path,
                             double_blind = TRUE,
                             prefix = "",
                             suffix = "",
                             message = NULL,
                             branch = "master",
                             overwrite = FALSE,
                             verbose = TRUE) {
  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr_scalar(message, allow_null = TRUE)
  arg_is_chr(local_path)
  arg_is_lgl(double_blind, overwrite, verbose)

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  file_status = fs::file_exists(local_path)
  if (any(!file_status))
    usethis::ui_stop(
      "Unable to locate the following file(s): {usethis::ui_value(local_path[!file_status])}"
    )

  aut = unique(rdf[['repo_aut']])

  purrr::walk(aut,
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
              })
}



#' Collect scores from review forms
#'
#' The `peer_score_review()` function collects score information from the YAML of a review form within reviewers' review repositories. It outputs a new .csv file, with rows specifying individual question scores for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param form_review Character. File name of reviewer feedback form (must be .Rmd document).
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' peer_score_review(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' form_review = "hw2_review.Rmd",
#' prefix = prefix)
#' }
#'
#' @importFrom rlang .data
#' @family peer review functions
#'
#' @export
#'
peer_score_review = function(org,
                             roster,
                             form_review,
                             prefix = "",
                             suffix = "",
                             write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, form_review)
  arg_is_lgl(write_csv)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", form_review)) {
    usethis::ui_stop("{usethis::ui_field('form_review')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  out_temp = purrr::map_dfr(seq_len(nrow(rdf)),
                            function(x) {
                              repo = as.character(rdf[x, 'repo_rev_review'])
                              ghpath = glue::glue("{as.character(rdf[x, 'aut_random'])}/{form_review}")
                              rev_no = as.character(rdf[x, 'rev_no'])

                              feedback = purrr::safely(repo_get_file)(repo = repo,
                                                                      path = ghpath)

                              if (succeeded(feedback)) {
                                tc = textConnection(feedback[['result']])
                                scores = rmarkdown::yaml_front_matter(tc)[['params']]
                                scores[scores == "[INSERT SCORE]"] = NA
                                scores = purrr::map_chr(scores, ~ gsub("[][]", "", .x))

                                inp = stats::setNames(c(as.character(rdf[x, 'aut']), rev_no, scores),
                                                      c("user", "rev_no", paste0("q", 1:length(scores))))
                                tibble::as_tibble(as.list(inp))

                              } else {
                                usethis::ui_oops(
                                  "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                )
                              }
                            })

  out_temp = tidyr::gather(
    out_temp,
    "q_name",
    "q_value",
    -.data$user,
    -.data$rev_no
  )
  out_temp = tidyr::unite(out_temp, "new", c("rev_no", "q_name"))
  out_temp = tidyr::spread(out_temp, .data$new, .data$q_value)
  out = merge(out_temp, roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out[['user_random']]),]

  if (write_csv) {
    prefix_for_fname = sub("-", "", prefix)
    fname = glue::glue('revscores-{prefix_for_fname}.csv')
    readr::write_csv(out, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    out
  }
}


#' Collect scores from rating forms
#'
#' The `peer_score_rating()` function collects score information from the YAML of a rating form within authors' repositories. It outputs a new .csv file, with rows specifying individual question scores for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param form_rating Character. File name of rating feedback form (must be .Rmd document).
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `TRUE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' peer_score_rating(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' form_rating = "hw2_rating.Rmd",
#' double_blind = TRUE,
#' prefix = prefix)
#' }
#'
#' @importFrom rlang .data
#' @family peer review functions
#'
#' @export
#'
peer_score_rating = function(org,
                             roster,
                             form_rating,
                             double_blind = TRUE,
                             prefix = "",
                             suffix = "",
                             write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, form_rating)
  arg_is_lgl(double_blind, write_csv)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", form_rating)) {
    usethis::ui_stop("{usethis::ui_field('form_rating')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  out_temp = purrr::map_dfr(seq_len(nrow(rdf)),
                            function(x) {
                              repo = as.character(rdf[x, 'repo_aut'])
                              if (double_blind) {
                                ghpath = glue::glue("{as.character(rdf[x, 'rev_no'])}/{form_rating}")
                              } else {
                                ghpath = glue::glue("{as.character(rdf[x, 'rev'])}/{form_rating}")
                              }
                              rev_no = as.character(rdf[x, 'rev_no'])

                              feedback = purrr::safely(repo_get_file)(repo = repo,
                                                                      path = ghpath)

                              if (succeeded(feedback)) {
                                tc = textConnection(feedback[['result']])
                                scores = rmarkdown::yaml_front_matter(tc)[['params']]
                                scores[scores == "[INSERT SCORE]"] = NA
                                scores = purrr::map_chr(scores, ~ gsub("[][]", "", .x))

                                inp = stats::setNames(c(as.character(rdf[x, 'rev']), rev_no, scores),
                                                      c("user", "rev_no", paste0("c", 1:length(scores))))

                                tibble::as_tibble(as.list(inp))

                              } else {
                                usethis::ui_oops(
                                  "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                )
                              }
                            })

  # Getting data frame in right format
  out_temp = tidyr::gather(
    out_temp,
    "q_name",
    "q_value",
    -.data$user,
    -.data$rev_no
  )
  out_temp = tidyr::unite(out_temp, "new", c("rev_no", "q_name"))
  out_temp = tidyr::spread(out_temp, .data$new, .data$q_value)
  out = merge(out_temp, roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out[['user_random']]),]

  if (write_csv) {
    fname = glue::glue("{autscores}_{fs::path_file(roster)}")
    readr::write_csv(out, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    out
  }
}


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
peer_return = function(org,
                       roster,
                       path,
                       form_review = NULL,
                       local_path_rating = NULL,
                       double_blind = TRUE,
                       prefix = "",
                       suffix = "",
                       message = NULL,
                       branch = "master",
                       overwrite = FALSE) {
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
  content_og = purrr::map(seq_along(repo_aut_og),
                          function(a) {
                            repo_path_content_grab(
                              repo = repo_aut_og[a],
                              path = path,
                              repo_files = repo_files_aut_og[[a]],
                              branch = branch
                            )
                          })

  # The `purrr::map_df` statement below goes through multiple steps in the
  # return review process: add rating forms (if applicable), copy authors'
  # original content into reviewer-specific folders on authors' repositories
  # (to create a difference view on GitHub for the author), copy select
  # assignment files from reviewer to author repositores, copy completed review
  # forms from reviewer to author repositores, and create an issue for the author.
  # We keep all the steps in a single iteration over author repositories
  # in order to reduce calls to the GitHub API, which are limited to 5000
  # per hour.
  out = purrr::map_df(seq_len(nrow(rdf)),
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

                      })

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
