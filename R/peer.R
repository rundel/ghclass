## !!! Remember to run usethis::use_pipe() before PR !!!
# Also, remember to change parameter to repo_folder inside call to repo_put_file()

# Helper function for Latin square
g = function(j, n) {
  i <- seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}


#' Create peer review roster
#'
#' `peer_create_roster` creates data frame of random assignments of author files to reviewers. By default, the output is saved to a `.csv` file in the current working directory that incorporates the current date and random seed as part of the file name.
#'
#' @param user Character. A vector of GitHub user names.
#' @param m Numeric. Number of reviews per user. Must be larger than zero and smaller than the number of users.
#' @param seed Numeric. Random seed for assignment, defaults to `12345`.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_create_roster(3, c("anya", "bruno", "celine", "diego"))
#' }
#'
#' @export
#'
peer_create_roster = function(m,
                              user,
                              seed = 12345,
                              write_csv = TRUE) {
  stopifnot(is.numeric(m))
  stopifnot(is.numeric(seed))
  arg_is_chr(user)
  stopifnot(length(user) > 1, m > 0, m < length(user))

  set.seed(seed)
  j = sample(2:length(user), m)
  res = purrr::map(j, ~ g(.x, length(user)))

  # Randomizing user names to avoid clustering
  user_random = paste0("aut", sample(1:length(user), length(user)))
  df_sort = data.frame(user = user,
                       user_random = as.character(user_random))[order(as.numeric(sub("[aA-zZ]+", "", user_random))), ]

  res_df = setNames(data.frame(df_sort,
                               do.call(
                                 cbind, purrr::map(res, ~ as.character(df_sort$user_random)[.x])
                               )),
                    c("user", "user_random", purrr::map_chr(1:m, ~ paste0("rev", .x))))

  res_df = res_df[order(res_df$user_random), ]

  if (write_csv) {
    fname = glue::glue("roster_seed{seed}.csv")
    readr::write_csv(res_df, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    tibble::as_tibble(purrr::modify_if(res_df, is.factor, as.character))
  }
}


format_prefix = function(x) {
  if (x != "" & !grepl("[\\w\\d]*[_-]$", x)) {
    paste0(x, "-")
  } else {
    x
  }
}

format_suffix = function(x) {
  if (x != "" & !grepl("^[_-][\\w\\d]*", x)) {
    paste0("-", x)
  } else {
    x
  }
}



# Reads roster file
peer_read_roster = function(roster,
                            prefix = NULL,
                            suffix = NULL) {
  res = purrr::safely(fs::file_exists)(roster)

  if (is.null(res$result) & is.data.frame(roster)) {
    rdf = tibble::as_tibble(purrr::modify_if(roster, is.factor, as.character))
  } else if (is.null(res$result) & !is.data.frame(roster)) {
    usethis::ui_stop("{usethis::ui_field('roster')} must be a data.frame or .csv file.")
  } else if (!res$result) {
    usethis::ui_stop("Cannot locate file: {usethis::ui_value(roster)}")
  } else if (res$result) {
    rdf = suppressMessages(readr::read_csv(roster))
  }

  rdf

}

# Checks whether necessary column names are present
peer_check_roster = function(roster) {
  val = c("user", "user_random")
  purrr::walk(val,
              function(val) {
                if (!(val %in% names(roster))) {
                  usethis::ui_oops("{usethis::ui_field('roster')} must contain column {usethis::ui_value(val)}")
                }
              })

  if (!(any(grepl("^rev[0-9]+$", names(roster))))) {
    usethis::ui_oops(
      "{usethis::ui_field('roster')} must contain at least one column {usethis::ui_field('rev*')}"
    )
  }
}



peer_get_reviewer = function(author,
                             roster,
                             out = c("reviewer",
                                     "reviewer_random",
                                     "reviewer_no")) {
  m = seq_len(length(names(roster)[grepl("^rev[0-9]+$", names(roster))]))
  reviewer_random = as.character(roster[roster$user == author, paste0("rev", m)])
  reviewer = roster$user[purrr::map_int(reviewer_random, ~ which(roster$user_random == .x))]
  reviewer_no = names(roster)[purrr::map_int(reviewer_random, ~ which(roster[roster$user == author,] == .x))]

  if (out == "reviewer") {
    reviewer
  } else if (out == "reviewer_random") {
    reviewer_random
  } else if (out == "reviewer_no") {
    reviewer_no
  }
}


#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to reviewers' repositories.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#' @param path Character. File name or vector of file names to be included.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message, defaults to "Assigning review."
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_assign(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' path = c("task.Rmd", "iris_data.csv"),
#' prefix = "hw2-"
#' )
#' }
#'
#' @export
#'
peer_assign = function(org,
                       roster,
                       path = NULL,
                       prefix = "",
                       suffix = "",
                       prefix_rev = "",
                       suffix_rev = "",
                       message = "Assigning review",
                       branch = "master",
                       overwrite = FALSE) {

  arg_is_chr(org, prefix, suffix, prefix_rev, suffix_rev, branch)
  arg_is_chr(path, message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix = format_prefix(prefix)
  prefix_rev = format_prefix(prefix_rev)
  suffix = format_suffix(suffix)
  suffix_rev = format_suffix(suffix_rev)

  rdf = peer_expand_roster(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  author = unique(rdf$author)

  purrr::walk(author,
              function(x) {

                sub = rdf[rdf$author == x,]

                # Get files
                if (is.null(path)) {

                  files = repo_files()


                }

                content = purrr::map(path,
                                     function(y) {
                                       res = purrr::safely(repo_get_file)(repo = unique(sub$repo_a),
                                                                          file = y,
                                                                          branch = branch)
                                       if (succeeded(res))
                                         res$result
                                     })


              })

  # author = as.list(as.character(rdf$user))
  # author_random = as.list(as.character(rdf$user_random))
  #
  # purrr::walk2(author, author_random,
  #              function(author, author_random) {
  #                repo1 = glue::glue("{org}/{prefix}{author}{suffix}")
  #
  #                # First, get file content(s)
  #                content = purrr::map(path,
  #                                     function(path) {
  #                                       res = purrr::safely(repo_get_file)(repo = repo1,
  #                                                                          file = path,
  #                                                                          branch = branch)
  #
  #                                       if (succeeded(res)) {
  #                                         res$result
  #                                       }
  #                                     })
  #
  #                # Grab reviewers
  #                reviewer = peer_get_reviewer(author, rdf, "reviewer")
  #
  #                # Create folder paths (from perspective of reviewers)
  #                path = as.list(glue::glue("{author_random}/{path}"))
  #
  #                purrr::walk(reviewer,
  #                            function(reviewer) {
  #                              repo2 = glue::glue("{org}/{prefix}{reviewer}{suffix}")
  #                              purrr::walk2(path, content,
  #                                           function(path, content) {
  #                                             if (!file_exists(repo2, path) |
  #                                                 overwrite == TRUE) {
  #                                               if (!is.null(content)) {
  #                                                 repo_put_file(
  #                                                   repo = repo2,
  #                                                   path = path,
  #                                                   content = content,
  #                                                   message = message,
  #                                                   branch = branch,
  #                                                   verbose = TRUE
  #                                                 )
  #                                               }
  #                                             } else {
  #                                               usethis::ui_oops(
  #                                                 "Failed to add {usethis::ui_value(path)} to {usethis::ui_value(repo2)}: already exists."
  #                                               )
  #                                             }
  #                                           })
  #                            })
  #              })
}





#' Create reviewer feedback form
#'
#' `peer_create_form_review` creates blank feedback forms for reviewers based on the user-specified number of questions.
#'
#' @param n Numerical. Number of score fields to be included in .Rmd YAML.
#' @param title Character. Title of form, defaults to "Reviewer feedback form."
#' @param filename Character. File name of RMarkdown document, defaults to `rfeedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param dblind Logical. If `dblind = TRUE`, the YAML will contain an `author` field, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_create_form_review(5, "Reviewer feedback for HW2", "rfeedback_hw2_blank")
#' }
#'
#' @export
#'
peer_create_form_review = function(n,
                                   title = "Reviewer feedback form",
                                   filename = "rfeedback_blank",
                                   output = "github_document",
                                   write_rmd = TRUE,
                                   overwrite = FALSE,
                                   dblind = FALSE) {
  stopifnot(!is.null(filename))
  if (grepl("\\s+", filename)) {
    filename = stringr::str_replace_all(filename, "\\s", "_")
  }
  if (grepl("\\.Rmd$", filename)) {
    filename = stringr::str_replace_all(filename, "\\.Rmd$", "")
  }

  # YAML
  yaml_txt = sprintf(if (!dblind) {
    "---\ntitle: \"%s\"\nauthor: NA\noutput: %s\nparams:\n%s\n---\n\n\n"
  } else {
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n"
  },
  title,
  output,
  paste(purrr::map_chr(1:n, function(x) {
    paste0("  q", x, "_score: NA")
  }),
  collapse = "\n"))


  # Body
  resp = "Your response goes here..."
  body_txt = paste(
    "## Instructions",
    "Enter your feedback for each question below. Please replace `NA`s in the `q*_score` fields in the YAML with the scores you give the author for each question.",
    "## Feedback",
    paste0(purrr::map(
      1:n,
      ~ paste0(
        sprintf("%1$i. Place Question %1$i text here. [max. xxx points]\n\n", .x),
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
    fname = paste0(filename, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, filename = fname)
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


#' Create author feedback form
#'
#' `peer_create_form_rating` creates a short feedback form for authors to rate the feedback they got from reviewers.
#'
#' @param category Character. Categories to be included in the feedback form, defaults to `c("helpfulness", "accuracy", "fairness")`.
#' @param title Character. Title of form, defaults to "Author feedback form."
#' @param filename Character. File name of RMarkdown document, defaults to `rfeedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_create_form_rating(c("accuracy", "fairness"))
#' }
#'
#' @export
#'
peer_create_form_rating = function(category = c("helpfulness", "accuracy", "fairness"),
                                   title = "Author feedback form",
                                   filename = "afeedback_blank",
                                   output = "github_document",
                                   write_rmd = TRUE,
                                   overwrite = FALSE) {
  arg_is_chr(category)
  stopifnot(all(category %in% c("helpfulness", "accuracy", "fairness")))

  stopifnot(!is.null(filename))
  if (grepl("\\s+", filename)) {
    filename = stringr::str_replace_all(filename, "\\s", "_")
  }
  if (grepl("\\.Rmd$", filename)) {
    filename = stringr::str_replace_all(filename, "\\.Rmd$", "")
  }

  # YAML
  yaml_txt = sprintf(
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n",
    title,
    output,
    paste(paste0("  ", category, ": NA"), collapse = "\n")
  )

  # Instructions
  instruct_txt = sprintf(
    "Please rate the reviewer's feedback based on the categories below on a scale from \"Strongly disagree\" to \"Strongly agree.\" Please replace `NA`s in the %s %s in the YAML with the scores you give the reviewer for each category.",
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

  # Putting it all together
  body_txt = paste("## Instructions",
                   instruct_txt,
                   paste(paste0(
                     1:length(category),
                     ". ",
                     purrr::map_chr(category, ~ paste(category_txt[which(names(category_txt) == .x)]))
                   ),
                   collapse = "\n\n"),
                   tab_txt,
                   sep = "\n\n")


  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")

  if (write_rmd) {
    fname = paste0(filename, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, filename = fname)
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
#' `peer_add_file_rev()` takes a local file and adds it to author-specific folders on reviewers' repositories. The function's main purpose is to distribute review forms into the folders containing copies of authors' files.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_form_review`.
#' @param local_path Character. File name of file to be added.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_add_file_rev(org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' local_path = "rfeedback_hw2_blank.Rmd",
#' prefix = revprefix)
#' }
#'
#' @export
peer_add_file_rev = function(org,
                             roster,
                             local_path,
                             prefix = "",
                             suffix = "",
                             message = NULL,
                             branch = "master",
                             overwrite = FALSE) {
  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr_scalar(message, allow_null = TRUE)
  arg_is_chr(local_path)
  arg_is_lgl(dblind, overwrite)

  rdf = peer_expand_roster(org, roster, prefix, suffix)

  # repo_add_file does modification check
  purrr::walk(seq_len(nrow(rdf)),
              function(x) {
                repo_add_file(
                  repo = as.character(rdf[x, 'repo_r']),
                  file = local_path,
                  repo_folder = as.character(rdf[x, 'author_random']),
                  preserve_path = FALSE,
                  overwrite = overwrite
                )

              })
}


#' Add local files to reviewer-specific folders on authors' repositories
#'
#' `peer_add_file_aut()` takes a local file and adds it to reviewer-specific folders on authors' repositories. The function's main purpose is to distribute rating forms into the folders containing copies of reviewers' files.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_form_review`.
#' @param local_path Character. File name of file to be added.
#' @param dblind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `dblind = TRUE`, reviewer folders are identified by the reviewer's ID. If `dblind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_add_file_aut(org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' local_path = "afeedback_hw2_blank.Rmd",
#' prefix = prefix)
#' }
#'
#' @export
peer_add_file_aut = function(org,
                             roster,
                             local_path,
                             dblind = FALSE,
                             prefix = "",
                             suffix = "",
                             message = NULL,
                             branch = "master",
                             overwrite = FALSE) {
  arg_is_chr_scalar(org, prefix, suffix)
  arg_is_chr_scalar(message, allow_null = TRUE)
  arg_is_chr(local_path)
  arg_is_lgl(dblind, overwrite)

  rdf = peer_expand_roster(org, roster, prefix, suffix)

  # repo_add_file does modification check
  purrr::walk(seq_len(nrow(rdf)),
              function(x) {
                if (!dblind) {
                  folder = as.character(rdf[x, 'reviewer'])
                } else {
                  folder = as.character(rdf[x, 'reviewer_no'])
                }

                repo_add_file(
                  repo = as.character(rdf[x, 'repo_a']),
                  file = local_path,
                  repo_folder = folder,
                  preserve_path = FALSE,
                  overwrite = overwrite
                )

              })
}





#' Collect scores from review forms
#'
#' The `peer_score_review()` function collects score information from the YAML of a review form within reviewers' review repositories. It outputs a new .csv file, with rows specifying individual question scores for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_form_review`.
#' @param path Character. File name of feedback form (must be .Rmd document).
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_score_review(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' path = "rfeedback_blank.Rmd",
#' prefix = revprefix)
#' }
#'
#' @export
#'
peer_score_review = function(org,
                             roster,
                             path,
                             prefix = "",
                             suffix = "",
                             write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, path)
  arg_is_lgl(dblind, write_csv)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", path)) {
    usethis::ui_stop("{usethis::ui_field('path')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  rdf = peer_expand_roster(org, roster, prefix, suffix)

  out = purrr::map_dfr(seq_len(nrow(rdf)),
                       function(x) {
                         repo = as.character(rdf[x, 'repo_r'])
                         ghpath = glue::glue("{as.character(rdf[x, 'author_random'])}/{path}")
                         r_no = as.character(rdf[x, 'reviewer_no'])

                         feedback = purrr::safely(repo_get_file)(repo = repo,
                                                                 file = ghpath)

                         if (succeeded(feedback)) {
                           tc = textConnection(feedback$result)
                           scores = rmarkdown::yaml_front_matter(tc)$params
                           scores[scores == "NA"] = NA

                           setNames(c(as.character(rdf[x, 'author']), r_no, scores),
                                    c("user", "r_no", paste0("q", 1:length(scores))))

                         } else {
                           usethis::ui_oops(
                             "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                           )
                         }
                       }) %>%
    # Getting data frame in right format
    tidyr::gather(q_name, q_value,-user,-r_no) %>%
    tidyr::unite("new", c("r_no", "q_name")) %>%
    tidyr::spread(new, q_value) %>%
    merge(roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out$user_random), ]

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
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_form_review`.
#' @param path Character. File name of feedback form (must be .Rmd document).
#' @param dblind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `dblind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `dblind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_score_rating(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' path = "afeedback_blank.Rmd",
#' dblind = TRUE,
#' prefix = prefix)
#' }
#'
#' @export
#'
peer_score_rating = function(org,
                             roster,
                             path,
                             dblind = FALSE,
                             prefix = "",
                             suffix = "",
                             write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, path)
  arg_is_lgl(dblind, write_csv)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", path)) {
    usethis::ui_stop("{usethis::ui_field('path')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  rdf = peer_expand_roster(org, roster, prefix, suffix)

  out = purrr::map_dfr(seq_len(nrow(rdf)),
                       function(x) {
                         repo = as.character(rdf[x, 'repo_a'])
                         if (dblind) {
                           ghpath = glue::glue("{as.character(rdf[x, 'reviewer_no'])}/{path}")
                         } else {
                           ghpath = glue::glue("{as.character(rdf[x, 'reviewer'])}/{path}")
                         }
                         r_no = as.character(rdf[x, 'reviewer_no_scorea'])

                         feedback = purrr::safely(repo_get_file)(repo = repo,
                                                                 file = ghpath)

                         if (succeeded(feedback)) {
                           tc = textConnection(feedback$result)
                           scores = rmarkdown::yaml_front_matter(tc)$params
                           scores[scores == "NA"] = NA

                           setNames(c(as.character(rdf[x, 'reviewer']), r_no, scores),
                                    c("user", "r_no", paste0("c", 1:length(scores))))

                         } else {
                           usethis::ui_oops(
                             "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                           )
                         }
                       }) %>%
    # Getting data frame in right format
    tidyr::gather(q_name, q_value,-user,-r_no) %>%
    tidyr::unite("new", c("r_no", "q_name")) %>%
    tidyr::spread(new, q_value) %>%
    merge(roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out$user_random), ]

  if (write_csv) {
    fname = glue::glue("{autscores}_{fs::path_file(roster)}")
    readr::write_csv(out, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    out
  }
}



peer_expand_roster = function(org,
                              roster,
                              prefix = "",
                              suffix = "",
                              prefix_rev = "",
                              suffix_rev = "") {

  arg_is_chr_scalar(prefix, suffix, prefix_rev, suffix_rev)

  rdf = peer_read_roster(roster)
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  out = suppressWarnings(purrr::map2_dfr(author, author_random,
                                         function(author, author_random) {
                                           tibble::tibble(
                                             author = author,
                                             author_random = author_random,
                                             repo_a = glue::glue("{org}/{prefix}{author}{suffix}"),
                                             reviewer = peer_get_reviewer(author, rdf, "reviewer"),
                                             reviewer_random = peer_get_reviewer(author, rdf, "reviewer_random"),
                                             reviewer_no = peer_get_reviewer(author, rdf, "reviewer_no"),
                                             repo_r = glue::glue("{org}/{prefix}{reviewer}{suffix}"),
                                             repo_rev_r = glue::glue("{org}/{prefix_rev}{reviewer}{suffix_rev}"),
                                             reviewer_no_scorea = names(rdf)[purrr::map_int(reviewer_random, ~
                                                                                              which(rdf[rdf$user_random == .x,] == author_random))]
                                           )
                                         }))

  out

}


#' Mirror file(s) between repos
#'
#' `repo_mirror_file` mirrors select file(s) between repositories.
#'
#' @param source_repo Character. Address of repository in "owner/name" format.
#' @param target_repo Character. Address of repository in "owner/name" format.
#' @param path Character or character vector. Name(s) of file(s) to be moved.
#' @param source_folder Character. Name of folder containing file on `source_repo`.
#' @param target_folder Character. Name of folder containing file on `target_repo`.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#'
repo_mirror_file = function(source_repo,
                            target_repo,
                            path,
                            source_folder = NULL,
                            target_folder = NULL,
                            message = NULL,
                            branch = "master",
                            overwrite = FALSE,
                            verbose = TRUE) {
  arg_is_chr(path)
  arg_is_chr_scalar(source_repo, target_repo)
  arg_is_chr_scalar(source_folder, target_folder, message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  source_files = repo_files(source_repo, branch)
  target_files = repo_files(target_repo, branch)

  purrr::walk(path,
              function(.p) {
                if (!is.null(source_folder)) {
                  source_path = glue::glue("{source_folder}/{.p}")
                } else {
                  source_path = .p
                }

                if (!is.null(target_folder)) {
                  target_path = glue::glue("{target_folder}/{.p}")
                } else {
                  target_path = .p
                }

                if (source_path %in% source_files[['path']]) {
                  if (!(target_path %in% target_files[['path']]) | overwrite) {
                    res = purrr::safely(repo_get_file)(source_repo, source_path)

                    if (succeeded(res)) {
                      repo_put_file(
                        repo = target_repo,
                        path = target_path,
                        content = res$result,
                        message = message,
                        branch = branch,
                        verbose = verbose
                      )
                    }
                  } else {
                    usethis::ui_oops(
                      paste(
                        'Failed to add {usethis::ui_value(target_path)} to {usethis::ui_value(target_repo)}: already exists.',
                        'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
                      )
                    )
                  }
                } else {
                  usethis::ui_oops(
                    "Failed to locate {usethis::ui_value(source_path)} on {usethis::ui_value(source_repo)}."
                  )
                }
              })
}

#' Return peer feedback to authors
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#' @param path Character. File name or vector of file names to be included.
#' @param dblind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `dblind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `dblind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param prefix_rev Character. Common repository name prefix for review repositories.
#' @param suffix_rev Character. Common repository name suffix for review repositories.
#' @param message Character. Commit message, defaults to "Assigning review."
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_return(org = "ghclass-test,
#' roster = "hw2_roster_seed12345.csv",
#' path = c("hw2_task.Rmd", "rfeedback_blank.Rmd"),
#' prefix = "hw2-,
#' dblind = T)
#' }
#'
#' @export
#'
peer_return = function(org,
                       roster,
                       path,
                       rfeedback = NULL,
                       afeedback = NULL,
                       dblind = FALSE,
                       prefix = "",
                       suffix = "",
                       prefix_rev = "",
                       suffix_rev = "",
                       message = NULL,
                       branch = "master",
                       overwrite = FALSE) {
  arg_is_chr(path)
  arg_is_chr_scalar(org, prefix, suffix, prefix_rev, suffix_rev, branch)
  arg_is_chr(message, rfeedback, afeedback, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  prefix = format_prefix(prefix)
  suffix = format_suffix(suffix)
  prefix_rev = format_prefix(prefix_rev)
  suffix_rev = format_suffix(suffix_rev)

  rdf = peer_expand_roster(org, roster, prefix, suffix, prefix_rev, suffix_rev)

  if (!dblind) {
    rev = rdf[['reviewer']]
  } else {
    rev = rdf[['reviewer_no']]
  }

  purrr::walk(seq_len(nrow(rdf)),
              function(x) {
                repo_a = rdf[['repo_a']][x]
                repo_r = rdf[['repo_rev_r']][x]

                # 1) place original content
                # FIX THIS!! Something is going on w messaging and/or putting files
                repo_mirror_file(
                  source_repo = repo_a,
                  target_repo = repo_a,
                  target_folder = rev[x],
                  path = path,
                  message = "Placing original file",
                  branch = branch,
                  overwrite = overwrite
                )

                # 2) move files from reviewer
                repo_mirror_file(
                  source_repo = repo_r,
                  target_repo = repo_a,
                  source_folder = rdf[['author_random']][x],
                  target_folder = rev[x],
                  path = c(path, rfeedback),
                  message = "Adding reviewer feedback",
                  branch = branch,
                  overwrite = TRUE,
                  verbose = TRUE
                )
              })

  # 3) Create issue
  peer_create_issue_rating(
    rdf = rdf,
    path = path,
    rfeedback = rfeedback,
    afeedback = afeedback,
    dblind = dblind
  )
}

#' @param rfeedback Character. File name of reviewer feedback form.
#' @param afeedback Character. File name of author feedback form.
#'
peer_create_issue_rating = function(rdf,
                                    path,
                                    rfeedback,
                                    afeedback,
                                    title = "Reviewer feedback",
                                    label = "test",
                                    dblind = FALSE) {
  purrr::walk(unique(rdf[['author']]),
              function(x) {
                sub = rdf[rdf[['author']] == x, ]

                res = purrr::safely(github_api_issue_create)(
                  repo = unique(sub[['repo_a']]),
                  title = title,
                  body = peer_issue_body_rating(sub, path, rfeedback, afeedback, dblind),
                  assignee = x,
                  labels = list(":mag: Inspect review")
                )

                status_msg(res,
                           glue::glue("Posted issue for {x}"),
                           glue::glue("Cannot post issue for {x}"))

              })
}


#' @param path Character. File name of file for which the difference should be created.
#'
peer_issue_body_rating = function(sub,
                                  path,
                                  rfeedback = NULL,
                                  afeedback = NULL,
                                  dblind) {
  arg_is_chr(path)
  arg_is_chr_scalar(rfeedback, afeedback, allow_null = T)

  repo_a = unique(sub[['repo_a']])
  url_start = glue::glue("https://github.com/{repo_a}/blob/master/")

  fdf = repo_files(repo_a)

  if (!dblind) {
    rev_sub = sub[['reviewer']]
  } else {
    rev_sub = sub[['reviewer_no']]
  }

  #https://github.com/ghclass-test/homework2-thereanders/blob/master/rev1/rfeedback_blank.Rmd

  out = purrr::map_dfr(rev_sub,
                       function(y) {
                         if (!is.null(afeedback)) {
                           atemp = fdf[['path']][grepl(y, fdf[['path']]) &
                                                   grepl(afeedback, fdf[['path']])]
                         }

                         if (!is.null(rfeedback)) {
                           rtemp = fdf[['path']][grepl(y, fdf[['path']]) &
                                                   grepl(rfeedback, fdf[['path']])]
                         }

                         cbind(
                           tibble::tibble(
                             reviewer = y,
                             afeed = ifelse(
                               !is.null(afeedback),
                               paste0(url_start, atemp),
                               character()
                             ),
                             rfeed = ifelse(
                               !is.null(rfeedback),
                               paste0(url_start, rtemp),
                               character()
                             )
                           ),
                           suppressWarnings(create_diff_url(repo_a, glue::glue("{y}/{path}")))
                           #get_lastcommiturl(repo_a, glue::glue("{.x}/{path}"))
                         )
                       })

  rev_txt = purrr::map_chr(rev_sub,
                           function(y) {
                             paste(
                               glue::glue("**For {y}**"),
                               expand_diff(out, path, y),
                               check_rfeed(out, y),
                               check_afeed(out, y),
                               sep = "\n"
                             )
                           })

  glue::glue(
    'The feedback from your peers has been added to your repository.\n\n',
    'To finish the assignment, please complete the following tasks for each of the reviewers:\n\n',
    paste(rev_txt, collapse = "\n\n")
  )

}

expand_diff = function(out, path, .x) {
  diff_txt = purrr::map_chr(seq_len(length(path)),
                            function(.y) {
                              glue::glue(
                                "- [ ] Review changes suggested for [{path[.y]}]({out[out$reviewer == .x, paste0('diff', .y)]})."
                              )
                            })
  paste(diff_txt, collapse = "\n")
}

check_rfeed = function(out, .x) {
  test = out[out$reviewer == .x, 'rfeed']
  if (!is.na(test)) {
    glue::glue("- [ ] Read [review]({test}).")
  }
}

check_afeed = function(out, .x) {
  test = out[out$reviewer == .x, 'afeed']
  if (!is.na(test)) {
    glue::glue("- [ ] Fill out [rating form]({test}).")
  }
}
