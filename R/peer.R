#' Create peer review roster
#'
#' `peer_roster_create` creates data frame of random assignments of author files to reviewers. By default, the output is saved to a `.csv` file in the current working directory that incorporates the current date and random seed as part of the file name.
#'
#' @param user Character. A vector of GitHub user names.
#' @param n_rev Numeric. Number of reviews per user. Must be larger than zero and smaller than the number of users.
#' @param seed Numeric. Random seed for assignment, defaults to `12345`.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' peer_roster_create(3, c("anya-ghclass", "bruno-ghclass", "celine-ghclass", "diego-ghclass"))
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_roster_create = function(n_rev,
                              user,
                              seed = 12345,
                              write_csv = TRUE) {
  stopifnot(is.numeric(n_rev))
  stopifnot(is.numeric(seed))
  arg_is_chr(user)
  stopifnot(length(user) > 1, n_rev > 0, n_rev < length(user))

  set.seed(seed)
  j = sample(2:length(user), n_rev)
  res = purrr::map(j, ~ latin_square(.x, length(user)))

  # Randomizing user names to avoid clustering
  # if length(user) == 2, will always res = c(2, 1)
  set.seed(seed)
  user_random = paste0("aut", sample(1:length(user), length(user)))

  df_sort = data.frame(user = user,
                       user_random = as.character(user_random))[order(as.numeric(sub("[aA-zZ]+", "", user_random))),]

  res_df = stats::setNames(data.frame(df_sort,
                                      if (length(user) > 2) {
                                        purrr::map(res,
                                                   ~ tibble::tibble(user_random = as.character(df_sort$user_random)[.x]))
                                      } else {
                                        rev(user_random)
                                      }),
                           c("user", "user_random", purrr::map_chr(1:n_rev, ~ paste0("rev", .x))))

  res_df = res_df[order(res_df$user_random),]

  if (write_csv) {
    fname = glue::glue("roster_seed{seed}.csv")
    readr::write_csv(res_df, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    tibble::as_tibble(purrr::modify_if(res_df, is.factor, as.character))
  }
}

#' Initiate peer review repositories
#'
#' `peer_init()` initiates peer review repositories. It creates a review repository for each user, adds users to their respective repositories, and applies peer review labels to all repositories (i.e. assignment and review repositories).
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
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
                     suffix = "") {
  arg_is_chr_scalar(org, prefix, suffix)

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)
  user = unique(rdf$reviewer)

  repo_create(org = org, name = user, prefix = prefix_rev, suffix = suffix_rev)
  repo_add_user(glue::glue("{org}/{prefix_rev}{user}{suffix_rev}"), user)

  peer_issue_label_apply(org = org,
                         repo = unique(c(rdf[['repo_a']], rdf[['repo_r_rev']])))

}

#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to review repositories. The function creates an issue on the reviewers' repositories informing them that the review files are available and creates links to the relevant documents.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param path Character. File name or vector of file names to be included. If `NULL`, all files not contained in folders, except `.gitignore`, `.Rhistory`, and `.Rproj`, will be moved to the reviewers' repositories.
#' @param local_path_review Character. Local file path of review feedback form to be added (must be .Rmd document), defaults to `NULL`. If `NULL`, no review form will be added to authors' repositories.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param exclude_extension Character. File extensions of files to not be moved to reviewer repositories if `path` is `NULL`, defaults to `c("gitignore", "md", "Rhistory", "Rproj")`.
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
                       exclude_extension = c("gitignore", "md", "Rhistory", "Rproj", "html"),
                       message = NULL,
                       branch = "master",
                       overwrite = FALSE) {
  arg_is_chr_scalar(org, prefix, suffix, branch)
  arg_is_chr_scalar(local_path_review, message, allow_null = TRUE)
  arg_is_chr(path, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  # Contents of blank review form from local file
  content_review = local_path_content_grab(local_path = local_path_review,
                                           check_rmd = TRUE)

  out = purrr::map_df(unique(rdf$author),
                      function(a) {
                        sub = rdf[rdf[['author']] == a, ]
                        repo_a = unique(sub[['repo_a']])
                        repo_r = unique(sub[['repo_r_rev']])
                        repo_files_a = repo_files(repo = repo_a, branch = branch)

                        repo_folder_r = unique(sub[['author_random']])
                        repo_files_r = repo_files(repo = repo_r, branch = branch)

                        # 1. Place rating form
                        # iterates over target_repo, but not content
                        if (length(content_review) > 0) {
                          rv = peer_add_content(
                            target_repo = repo_r,
                            target_folder = repo_folder_r,
                            target_files = repo_files_r,
                            content = content_review,
                            category = "review",
                            message = "Adding review form",
                            branch = branch,
                            overwrite = overwrite
                          )
                        }

                        # 2. Mirror files to reviewers
                        ## i.  Select paths
                        if (is.null(path)) {
                          path = repo_files_select(repo = repo_a,
                                                   repo_files = repo_files_a,
                                                   exclude_extension = exclude_extension,
                                                   branch = branch)
                        }

                        ## ii. Grab content
                        content_repo = repo_path_content_grab(repo = repo_a,
                                                              path = path,
                                                              repo_files = repo_files_a,
                                                              branch = branch)

                        if (length(content_review) > 0) {
                          repo_files_r_patch = unique(rbind(repo_files_r,
                                                            rv[names(rv) %in% names(repo_files_r)]))
                        } else {
                          repo_files_r_patch = repo_files_r
                        }

                        ## iii. Place content
                        mr = peer_add_content(
                          target_repo = repo_r,
                          target_folder = repo_folder_r,
                          target_files = repo_files_r_patch,
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

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  file_status = fs::file_exists(local_path)
  if (any(!file_status))
    usethis::ui_stop(
      "Unable to locate the following file(s): {usethis::ui_value(local_path[!file_status])}"
    )

  rev = unique(rdf[['repo_r_rev']])

  purrr::walk(rev,
              function(x) {
                repo_files = repo_files(x, branch = branch)
                aut = rdf[['author_random']][rdf[['repo_r_rev']] == x]

                peer_file_place(
                  repo_files = repo_files,
                  target_repo = x,
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

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  file_status = fs::file_exists(local_path)
  if (any(!file_status))
    usethis::ui_stop(
      "Unable to locate the following file(s): {usethis::ui_value(local_path[!file_status])}"
    )

  aut = unique(rdf[['repo_a']])

  purrr::walk(aut,
              function(x) {
                repo_files = repo_files(x)
                if (!double_blind) {
                  rev = rdf[['reviewer']][rdf[['repo_a']] == x]
                } else {
                  rev = rdf[['reviewer_no']][rdf[['repo_a']] == x]
                }

                peer_file_place(
                  repo_files = repo_files,
                  target_repo = x,
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

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  out_temp = purrr::map_dfr(seq_len(nrow(rdf)),
                            function(x) {
                              repo = as.character(rdf[x, 'repo_r_rev'])
                              ghpath = glue::glue("{as.character(rdf[x, 'author_random'])}/{form_review}")
                              r_no = as.character(rdf[x, 'reviewer_no'])

                              feedback = purrr::safely(repo_get_file)(repo = repo,
                                                                      file = ghpath)

                              if (succeeded(feedback)) {
                                tc = textConnection(feedback$result)
                                scores = rmarkdown::yaml_front_matter(tc)$params
                                scores[scores == "[INSERT SCORE]"] = NA
                                scores = purrr::map_chr(scores, ~gsub("[][]", "", .x))

                                inp = stats::setNames(c(as.character(rdf[x, 'author']), r_no, scores),
                                                      c("user", "r_no", paste0("q", 1:length(scores))))
                                tibble::as_tibble(as.list(inp))

                              } else {
                                usethis::ui_oops(
                                  "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                )
                              }
                            })

  out_temp = tidyr::gather(out_temp, .data$q_name, .data$q_value, -.data$user, -.data$r_no)
  out_temp = tidyr::unite(out_temp, .data$new, c("r_no", "q_name"))
  out_temp = tidyr::spread(out_temp, .data$new, .data$q_value)
  out = merge(out_temp, roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out$user_random),]

  if (write_csv) {
    fname = glue::glue("{revscores}_{fs::path_file(roster)}")
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

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  out_temp = purrr::map_dfr(seq_len(nrow(rdf)),
                            function(x) {
                              repo = as.character(rdf[x, 'repo_a'])
                              if (double_blind) {
                                ghpath = glue::glue("{as.character(rdf[x, 'reviewer_no'])}/{form_rating}")
                              } else {
                                ghpath = glue::glue("{as.character(rdf[x, 'reviewer'])}/{form_rating}")
                              }
                              r_no = as.character(rdf[x, 'reviewer_no'])

                              feedback = purrr::safely(repo_get_file)(repo = repo,
                                                                      file = ghpath)

                              if (succeeded(feedback)) {
                                tc = textConnection(feedback$result)
                                scores = rmarkdown::yaml_front_matter(tc)$params
                                scores[scores == "[INSERT SCORE]"] = NA
                                scores = purrr::map_chr(scores, ~gsub("[][]", "", .x))

                                inp = stats::setNames(c(as.character(rdf[x, 'reviewer']), r_no, scores),
                                                      c("user", "r_no", paste0("c", 1:length(scores))))

                                tibble::as_tibble(as.list(inp))

                              } else {
                                usethis::ui_oops(
                                  "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                )
                              }
                            })

  # Getting data frame in right format
  out_temp = tidyr::gather(out_temp, .data$q_name, .data$q_value, -.data$user, -.data$r_no)
  out_temp = tidyr::unite(out_temp, .data$new, c("r_no", "q_name"))
  out_temp = tidyr::spread(out_temp, .data$new, .data$q_value)
  out = merge(out_temp, roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out$user_random),]

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

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  if (!double_blind) {
    rev = rdf[['reviewer']]
  } else {
    rev = rdf[['reviewer_no']]
  }

  # Contents of blank rating form from local file
  content_rating = local_path_content_grab(local_path_rating)

  # Take snapshot of reviewers' review repos
  repo_r_og = unique(rdf[['repo_r_rev']])
  repo_files_r_og = purrr::map(repo_r_og, ~ repo_files(.x))

  # Take snapshot of authors' repos
  repo_a_og = unique(rdf[['repo_a']])
  repo_files_a_og = purrr::map(repo_a_og, ~ repo_files(.x))

  # get original content
  content_og = purrr::map(seq_along(repo_a_og),
                          function(a) {
                            repo_path_content_grab(
                              repo = repo_a_og[a],
                              path = path,
                              repo_files = repo_files_a_og[[a]],
                              branch = branch
                            )
                          })

  out = purrr::map_dfr(seq_len(nrow(rdf)),
                       function(x) {
                         repo_a = rdf[['repo_a']][x]
                         n_a = which(repo_a_og == repo_a)
                         repo_files_a = repo_files_a_og[[n_a]]

                         repo_r = rdf[['repo_r_rev']][x]
                         n_r = which(repo_r_og == repo_r)
                         repo_folder_r = rdf[['author_random']][x]
                         repo_files_r = repo_files_r_og[[n_r]]

                         # 1. Place rating form if applicable
                         # What if no rating form
                         # Check if the vectorized version still works
                         if (length(content_rating) > 0) {
                           rt = peer_add_content(
                             target_repo = repo_a,
                             target_folder = rev[x],
                             target_files = repo_files_a,
                             content = content_rating,
                             category = "rating",
                             message = "Adding rating form",
                             branch = branch,
                             overwrite = overwrite
                           )
                         } else {
                           rt = NULL
                         }

                         if (length(content_rating) > 0) {
                           repo_files_a_patch = unique(rbind(repo_files_a,
                                                             rt[names(rt) %in% names(repo_files_a)]))
                         } else {
                           repo_files_a_patch = repo_files_a
                         }

                         # 2. place original content
                         og = peer_add_content(
                           target_repo = repo_a,
                           target_folder = rev[x],
                           target_files = repo_files_a_patch,
                           content = content_og[[n_a]],
                           category = "original",
                           message = "Adding original file",
                           branch = branch,
                           overwrite = TRUE
                         )

                         # 3. Move assignment files from reviewer
                         # (difference will be created from this)
                         ## i. Grab content from review repos & remove folder
                         path_folder = glue::glue("{repo_folder_r}/{path}")
                         content_folder = content_path_folder_strip(
                           repo_path_content_grab(
                             repo = repo_r,
                             path = path_folder,
                             repo_files = repo_files_r,
                             branch = branch
                           ),
                           repo_folder_r
                         )

                         repo_files_a_patch2 = unique(rbind(repo_files_a_patch,
                                                            og[names(og) %in% names(repo_files_a_patch)]))

                         ## ii. Place content
                         mv = peer_add_content(
                           target_repo = repo_a,
                           target_folder = rev[x],
                           target_files = repo_files_a_patch2,
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
                           repo_files_a_patch3 = unique(rbind(repo_files_a_patch2,
                                                              mv[names(mv) %in% names(repo_files_a_patch2)]))

                           path_review = glue::glue("{repo_folder_r}/{form_review}")
                           content_review = content_path_folder_strip(
                             repo_path_content_grab(
                               repo = repo_r,
                               path = path_review,
                               repo_files = repo_files_r,
                               branch = branch
                             ),
                             repo_folder_r
                           )

                           ## ii. Place content
                           rv = peer_add_content(
                             target_repo = repo_a,
                             target_folder = rev[x],
                             target_files = repo_files_a_patch3,
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
