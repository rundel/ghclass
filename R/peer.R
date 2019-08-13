# Helper function for Latin square
latin_square = function(j, n) {
  i <- seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}


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

  repo_create(org, user, prefix = prefix_rev, suffix = suffix_rev)
  repo_add_user(glue::glue("{org}/{prefix_rev}{user}{suffix_rev}"), user)

  peer_apply_label(org = org)

}

format_rev = function(prefix, suffix) {
  tag = "review"
  if (prefix != "" & suffix == "") {
    list(prefix_rev = paste0(prefix, tag, "-"),
         suffix_rev = suffix)
  } else {
    list(prefix_rev = prefix,
         suffix_rev = paste0(suffix, "-", tag))
  }
}




#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to review repositories. It also creates an issue in the reviewers' repositories informing them that the review files are available and with links to the relevant documents.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param path Character. File name or vector of file names to be included. If `NULL`, all files not contained in folders, except `.gitignore`, `.Rhistory`, and `.Rproj`, will be moved to the reviewers' repositories.
#' @param form_review Character. File name of customized review feedback form. If `NULL`, no link will be created in the issue informing reviewers that the review files are available.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
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
                       form_review = NULL,
                       prefix = "",
                       suffix = "",
                       message = "Assigning review",
                       branch = "master",
                       overwrite = FALSE) {
  arg_is_chr(org, prefix, suffix, branch)
  arg_is_chr(form_review, path, message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  purrr::walk(unique(rdf$author),
              function(x) {
                sub = rdf[rdf$author == x, ]
                repo_a = unique(sub$repo_a)
                repo_r = unique(sub$repo_r_rev)

                # Get files (exclude .gitignore, exclude folders)
                if (is.null(path)) {
                  files_a = repo_files(repo = repo_a, branch = branch)
                  path = files_a$path[(files_a$type == "blob") &
                                        (!grepl("/", files_a$path)) &
                                        (!grepl("\\.Rproj$", files_a$path)) &
                                        !(files_a$path %in% c(".gitignore",
                                                              ".Rhistory"))]
                }

                content = purrr::map(path,
                                     function(path) {
                                       res = purrr::safely(repo_get_file)(repo = unique(sub$repo_a),
                                                                          file = path,
                                                                          branch = branch)
                                       if (succeeded(res))
                                         res$result
                                     })

                path_r = glue::glue("{unique(sub$author_random)}/{path}")

                purrr::walk(repo_r,
                            function(repo_r) {
                              files_r = repo_files(repo_r, branch)

                              purrr::walk2(content, path_r,
                                           function(content, path_r) {
                                             if (!(path_r %in% files_r[['path']]) | overwrite) {
                                               if (!is.null(content)) {
                                                 repo_put_file(
                                                   repo = repo_r,
                                                   path = path_r,
                                                   content = content,
                                                   message = message,
                                                   branch = branch,
                                                   verbose = T
                                                 )
                                               }
                                             } else {
                                               usethis::ui_oops(
                                                 "Failed to add {usethis::ui_value(path_r)} to {usethis::ui_value(repo_r)}: already exists."
                                               )
                                             }
                                           })
                            })

              })

  # Create issue
  peer_create_issue_review(rdf = rdf,
                           form_review = form_review)

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
#' @param double_blind Logical. If `double_blind = TRUE`, the YAML will contain an `author` field, defaults to `FALSE`.
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
                                   double_blind = FALSE) {
  stopifnot(!is.null(fname))
  if (grepl("\\s+", fname)) {
    fname = stringr::str_replace_all(fname, "\\s", "_")
  }
  if (grepl("\\.Rmd$", fname)) {
    fname = stringr::str_replace_all(fname, "\\.Rmd$", "")
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
    fname = stringr::str_replace_all(fname, "\\s", "_")
  }
  if (grepl("\\.Rmd$", fname)) {
    fname = stringr::str_replace_all(fname, "\\.Rmd$", "")
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
    comment_txt = "\n---\nIf you have additional comments for the reviewer, please enter it below.\n\n[Your comments go here]"
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

                peer_place_file(
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
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the reviewer's ID. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
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
                             double_blind = FALSE,
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

                peer_place_file(
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
                                scores = stringr::str_replace_all(scores, "[\\[\\]]", "")

                                stats::setNames(c(as.character(rdf[x, 'author']), r_no, scores),
                                                c("user", "r_no", paste0("q", 1:length(scores))))

                              } else {
                                usethis::ui_oops(
                                  "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                )
                              }
                            })

  out_temp = tidyr::gather(out_temp, q_name, q_value,-user,-r_no)
  out_temp = tidyr::unite(out_temp, "new", c("r_no", "q_name"))
  out_temp = tidyr::spread(out_temp, new, q_value)
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
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
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
                             double_blind = FALSE,
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
                                scores = stringr::str_replace_all(scores, "[\\[\\]]", "")

                                stats::setNames(c(as.character(rdf[x, 'reviewer']), r_no, scores),
                                                c("user", "r_no", paste0("c", 1:length(scores))))

                              } else {
                                usethis::ui_oops(
                                  "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                )
                              }
                            })

  # Getting data frame in right format
  out_temp = tidyr::gather(out_temp, q_name, q_value, -user, -r_no)
  out_temp = tidyr::unite(out_temp, "new", c("r_no", "q_name"))
  out_temp = tidyr::spread(out_temp, new, q_value)
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
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param path Character. File name or vector of file names to be included.
#' @param form_review Character. File name of reviewer feedback form (must be .Rmd document). If `NULL`, no link to the form will be created in the issue filed for authors.
#' @param form_rating Character. File name of rating feedback form (must be .Rmd document). If `NULL`, no link to the form will be created in the issue filed for authors.
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
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
#' form_rating = "hw2_rating.Rmd",
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
                       form_rating = NULL,
                       double_blind = FALSE,
                       prefix = "",
                       suffix = "",
                       message = NULL,
                       branch = "master",
                       overwrite = FALSE) {
  arg_is_chr(path)
  arg_is_chr_scalar(org, prefix, suffix, branch)
  arg_is_chr(message, form_review, form_rating, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix_rev = format_rev(prefix, suffix)$prefix_rev
  suffix_rev = format_rev(prefix, suffix)$suffix_rev

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  if (!double_blind) {
    rev = rdf[['reviewer']]
  } else {
    rev = rdf[['reviewer_no']]
  }

  out = purrr::map_dfr(seq_len(nrow(rdf)),
                       function(x) {
                         repo_a = rdf[['repo_a']][x]
                         repo_r = rdf[['repo_r_rev']][x]

                         # 1) place original content
                         og = peer_mirror_original(
                           source_repo = repo_a,
                           path = path,
                           target_folder = rev[x],
                           message = "Placing original file",
                           branch = branch,
                           overwrite = overwrite
                         )

                         # 2) Move files from reviewer
                         mv = peer_mirror_review(
                           og_content = og,
                           source_repo = repo_r,
                           target_repo = repo_a,
                           path = path,
                           form_review = form_review,
                           source_folder = rdf[['author_random']][x],
                           target_folder = rev[x],
                           message = "Adding reviewer feedback",
                           branch = branch
                         )

                         mv

                       })



  # 3) Create issue
  # peer_create_issue_rating(
  #   rdf = rdf,
  #   path = path,
  #   form_review = form_review,
  #   form_rating = form_rating,
  #   double_blind = double_blind
  # )
}



#' Mirror original file(s)
#'
#' `peer_mirror_original` mirrors original file(s) from an author's repository into a reviewer-specific folder on the author's repository and returns a list of contents.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param path Character or character vector. Name(s) of file(s) to be moved.
#' @param target_folder Character. Name of folder containing file on `target_repo`.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to `master`.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param verbose Logical. Should success/failure messages be printed, defaults to `TRUE`.
#'
#' @family peer review functions
peer_mirror_original = function(source_repo,
                                path,
                                target_folder,
                                message = NULL,
                                branch = "master",
                                overwrite = FALSE,
                                verbose = TRUE) {
  arg_is_chr(path)
  arg_is_chr_scalar(source_repo)
  arg_is_chr_scalar(target_folder,
                    message,
                    allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  source_files = repo_files(source_repo, branch)
  target_files = source_files[grepl(glue::glue("{target_folder}/"), source_files[['path']]),]

  out = purrr::map(path,
                   function(p) {
                     source_path = p
                     target_path = format_folder(target_folder, p)

                     if (p %in% source_files[['path']]) {
                       res = purrr::safely(repo_get_file)(source_repo, source_path)

                       if (succeeded(res)) {
                         target_exists = target_path %in% target_files[['path']]

                         if (target_exists & !overwrite) {
                           usethis::ui_oops(
                             paste(
                               'Failed to add {usethis::ui_value(target_path)} to {usethis::ui_value(target_repo)}: already exists.',
                               'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
                             )
                           )

                         } else {
                           if (target_exists) {
                             sha = target_files[['sha']][target_files[['path']] == target_path]
                           } else {
                             sha = NULL
                           }

                           peer_repo_put_file(
                             repo = source_repo,
                             path = target_path,
                             content = res[['result']],
                             message = message,
                             branch = branch,
                             sha = sha,
                             verbose = verbose
                           )

                           res[['result']]

                         }

                       } else {
                         usethis::ui_oops(
                           "Failed to locate {usethis::ui_value(source_path)} on {usethis::ui_value(source_repo)}"
                         )
                         NULL
                       }
                     }
                   })

  out

}


#' Mirror reviewer file(s)
#'
#' `peer_mirror_review` mirrors user-specified file(s) (and review forms if applicable) from a reviewer's review repository into a reviewer-specific folder on the author's repository and returns a list of URLs. The function conducts a check whether author assignment files were changed by the reviewer; the function returns `NULL` if there is no change.
#'
#' @param og_content Character. List of files with the content of an author's original assignment file(s)
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. Address of repository in "owner/name" format.
#' @param path Character or character vector. Name(s) of file(s) to be moved.
#' @param form_review Character. File name of reviewer feedback form (must be .Rmd document). If `NULL`, no review form will be moved.
#' @param source_folder Character. Name of folder containing file on `source_repo`.
#' @param target_folder Character. Name of folder containing file on `target_repo`.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to `master`.
#' @param verbose Logical. Should success/failure messages be printed, defaults to `TRUE`.
#'
#' @family peer review functions
peer_mirror_review = function(og_content,
                              source_repo,
                              target_repo,
                              path,
                              form_review = NULL,
                              source_folder = NULL,
                              target_folder = NULL,
                              message = NULL,
                              branch = "master",
                              verbose = TRUE) {
  arg_is_chr(path)
  arg_is_chr_scalar(source_repo, target_repo)
  arg_is_chr_scalar(source_folder,
                    target_folder,
                    message,
                    form_review,
                    allow_null = TRUE)

  source_files = repo_files(source_repo, branch)
  target_files = repo_files(target_repo, branch)

  if (!is.null(form_review)) {
    gh_path = c(path, form_review)
    autfile = c(form_review != gh_path)
  } else {
    gh_path = path
    autfile = rep(T, length(path))
  }

  out = purrr::map_dfr(seq_len(length(gh_path)),
                       function(p) {
                         source_path = format_folder(source_folder, gh_path[p])
                         target_path = format_folder(target_folder, gh_path[p])

                         if (source_path %in% source_files[['path']]) {
                           res = purrr::safely(repo_get_file)(source_repo, source_path)

                           if (succeeded(res)) {
                             target_exists = target_path %in% target_files[['path']]

                             if (target_exists) {
                               sha = target_files[['sha']][target_files[['path']] == target_path]
                             } else {
                               sha = NULL
                             }

                             # Checking for changes by reviewer
                             if (autfile[p] &
                                 !is.null(og[p]) &
                                 isTRUE(all.equal(og[p][[1]][1], res$result[[1]][1]))) {
                               usethis::ui_oops(
                                 "Skipping mirroring {usethis::ui_value(path[p])} from {usethis::ui_value(source_repo)} to {usethis::ui_value(target_repo)}: no changes detected."
                               )
                               changed = FALSE
                               added = FALSE
                               commit_sha = NA
                             } else {

                               if(!autfile[p]) {
                                 changed = NA
                               } else {
                                 changed = TRUE
                               }

                               res2 = peer_repo_put_file(
                                 repo = target_repo,
                                 path = target_path,
                                 content = res[['result']],
                                 message = message,
                                 branch = branch,
                                 sha = sha,
                                 verbose = verbose
                               )

                               if (succeeded(res2)) {
                                 added = TRUE
                                 commit_sha = res2[["result"]][["commit"]][["sha"]]
                               } else {
                                 added = FALSE
                                 commit_sha = NA
                               }
                             }

                             tibble::tibble(
                               repo = target_repo,
                               target_path = target_folder,
                               path = gh_path[p],
                               changed = changed,
                               added = added,
                               sha = ifelse(is.null(sha), NA, sha),
                               commit_sha = commit_sha
                             )

                           } else {
                             usethis::ui_oops(
                               "Failed to locate {usethis::ui_value(source_path)} on {usethis::ui_value(source_repo)}"
                             )
                           }
                         }
                       })
}
