## !!! Remember to run usethis::use_pipe() before PR !!!

# Helper function for Latin square
g = function(j, n) {
  i <- seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}


#' Create peer review roster
#'
#' `peer_roster` creates data frame of random assignments of author files to reviewers. By default, the output is saved to a `.csv` file in the current working directory that incorporates the current date and random seed as part of the file name.
#'
#' @param user Character. A vector of GitHub user names.
#' @param m Numeric. Number of reviews per user. Must be larger than zero and smaller than the number of users.
#' @param seed Numeric. Random seed for assignment, defaults to `12345`.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_roster(3, c("anya", "bruno", "celine", "diego"))
#' }
#'
#' @export
#'
peer_roster = function(m,
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
  user_random = paste0("a", sample(1:length(user), length(user)))
  df_sort = data.frame(user = user,
                       user_random = as.character(user_random))[order(as.numeric(sub("[aA-zZ]+", "", user_random))), ]

  res_df = setNames(data.frame(df_sort,
                               do.call(
                                 cbind, purrr::map(res, ~ as.character(df_sort$user_random)[.x])
                               )),
                    c("user", "user_random", purrr::map_chr(1:m, ~ paste0("r", .x))))

  if (write_csv) {
    fname = glue::glue("roster_seed{seed}.csv")
    readr::write_csv(res_df, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    tibble::as_tibble(purrr::modify_if(res_df, is.factor, as.character))
  }
}


# The following two functions (peer_anonymize_file and remove_author_rmd) are prob not needed any longer if we decide not to include author as a YAML parameter

# If we keep this function, it should just strip the author field from YAML
peer_anonymize_file = function(file) {
  remove_author_rmd(file)
}

remove_author_rmd = function(input) {
  sub(
    '\\nauthor: \\"[aA-zZ]+ ([aA-zZ]+[ \\.]+)?[aA-zZ]+\"',
    '\\nauthor: \\"Student x"',
    input
  )
}

# Reads roster file
peer_read_roster = function(roster, fname_append = NULL, prefix = NULL, suffix = NULL) {

  res = purrr::safely(fs::file_exists)(roster)

  if (is.null(res$result) & is.data.frame(roster)) {
    rdf = tibble::as_tibble(purrr::modify_if(roster, is.factor, as.character))
    fname = glue::glue("{prefix}{fname_append}{suffix}")
  } else if (is.null(res$result) & !is.data.frame(roster)) {
    usethis::ui_stop("{usethis::ui_field('roster')} must be a data.frame or .csv file.")
  } else if (!res$result) {
    usethis::ui_stop("Cannot locate file: {usethis::ui_value(roster)}")
  } else if (res$result) {
    rdf = suppressMessages(readr::read_csv(roster))
    fname = glue::glue("{fname_append}_{fs::path_file(roster)}")
  }

  list("rdf" = rdf, "fname" = fname)

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

  if (!(any(grepl("^r[0-9]+$", names(roster))))) {
    usethis::ui_oops(
      "{usethis::ui_field('roster')} must contain at least one column {usethis::ui_field('r*')}"
    )
  }
}



peer_get_reviewer = function(author, roster, anonym = FALSE) {

  m = seq_len(length(names(roster)[grepl("^r[0-9]+$", names(roster))]))
  reviewer_random = as.character(roster[roster$user == author, paste0("r", m)])
  reviewer = roster$user[purrr::map_int(reviewer_random, ~ which(roster$user_random == .x))]

  if (!anonym) {
    reviewer
  } else {
    reviewer_random
  }
}


#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to reviewers' repositories.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#' @param file Character. File name or vector of file names to be included.
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
#' file = c("task.Rmd", "iris_data.csv"),
#' prefix = "hw2-"
#' )
#' }
#'
#' @export
#'
peer_assign = function(org,
                       roster,
                       file,
                       prefix = "",
                       suffix = "",
                       message = "Assigning review",
                       branch = "master",
                       overwrite = FALSE) {
  arg_is_chr(org, file, prefix, suffix, branch)
  arg_is_chr(message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  rdf = peer_read_roster(roster)$rdf
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  purrr::walk2(author, author_random,
               function(author, author_random) {
                 repo1 = glue::glue("{org}/{prefix}{author}{suffix}")

                 # First, get file content(s)
                 content = purrr::map(file,
                                      function(file) {
                                        check = file_exists(repo = repo1, file = file)
                                        if(check){
                                          res = purrr::safely(get_file)(repo = repo1,
                                                                        file = file,
                                                                        branch = branch)
                                          if (succeeded(res)) {
                                            return(res$result)
                                          }
                                        }
                                      })

                 # Grab reviewers
                 reviewer = peer_get_reviewer(author, rdf, anonym = FALSE)

                 # Create folder paths (from perspective of reviewers)
                 path = as.list(glue::glue("{author_random}/{file}"))

                 purrr::walk(reviewer,
                             function(reviewer) {
                               repo2 = glue::glue("{org}/{prefix}{reviewer}{suffix}")
                               purrr::walk2(path, content,
                                            function(path, content) {
                                              if (!file_exists(repo2, path, verbose = FALSE) |
                                                  overwrite == TRUE) {
                                                if (!is.null(content)) {
                                                  put_file(
                                                    repo = repo2,
                                                    path = path,
                                                    content = content,
                                                    message = message,
                                                    branch = branch,
                                                    verbose = TRUE
                                                  )
                                                }
                                              } else {
                                                usethis::ui_oops(
                                                  "Failed to add {usethis::ui_value(path)} to {usethis::ui_value(repo2)}: already exists."
                                                )
                                              }
                                            })
                             })
               })
}





#' Create reviewer feedback form
#'
#' `peer_create_rform` creates blank feedback forms for reviewers based on the user-specified number of questions.
#'
#' @param n Numerical. Number of score fields to be included in .Rmd YAML.
#' @param title Character. Title of form, defaults to "Reviewer feedback form."
#' @param file Character. File name of RMarkdown document, defaults to `rfeedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param dblind Logical. If `dblind = TRUE`, the YAML will contain an `author` field, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_create_rform(5, "Reviewer feedback for HW2", "rfeedback_hw2_blank")
#' }
#'
#' @export
#'
peer_create_rform = function(n,
                             title = "Reviewer feedback form",
                             file = "rfeedback_blank",
                             output = "github_document",
                             write_rmd = TRUE,
                             overwrite = FALSE,
                             dblind = FALSE) {
  stopifnot(!is.null(file))
  if (grepl("\\s+", file)) {
    file = stringr::str_replace_all(file, "\\s", "_")
  }
  if (grepl("\\.Rmd$", file)) {
    file = stringr::str_replace_all(file, "\\.Rmd$", "")
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
        sprintf("%1$i. Place Question %1$i text here.\n\n", .x),
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
    fname = paste0(file, ".Rmd")
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


#' Create author feedback form
#'
#' `peer_create_aform` creates a short feedback form for authors to rate the feedback they got from reviewers.
#'
#' @param category Character. Categories to be included in the feedback form, defaults to `c("helpfulness", "accuracy", "fairness")`.
#' @param title Character. Title of form, defaults to "Author feedback form."
#' @param file Character. File name of RMarkdown document, defaults to `rfeedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_create_aform(c("accuracy", "fairness"))
#' }
#'
#' @export
#'
peer_create_aform = function(category = c("helpfulness", "accuracy", "fairness"),
                             title = "Author feedback form",
                             file = "afeedback_blank",
                             output = "github_document",
                             write_rmd = TRUE,
                             overwrite = FALSE) {
  stopifnot(!is.null(file))
  if (grepl("\\s+", file)) {
    file = stringr::str_replace_all(file, "\\s", "_")
  }
  if (grepl("\\.Rmd$", file)) {
    file = stringr::str_replace_all(file, "\\.Rmd$", "")
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

  category_txt = list(helpfulness = "`helpfulness`: \"The reviewer's feedback was constructive and helpful.\"",
                      accuracy = "`accuracy`: \"The reviewer's assessment accurately describes the quality of my work.\"",
                      fairness = "`fairness`: \"The reviewer's assessment was fair.\"")

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
    fname = paste0(file, ".Rmd")
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

#' Add files to repositories based on roster
#'
#' `peer_add_file()` is the peer review version of `repo_add_file()`. It takes a local file and adds it to author- or reviewer-specific folders in students' repositories based on the peer review roster. The function's main purpose is to distribute feedback forms into the folders containing copies of authors' or reviewers' files.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_rform`.
#' @param to Character. Specifies whether the file is to be added to reviewers' (`r`) or authors' (`a`) folders. If `to = "r"`, the function places the file into each of the folders containing the anonymized author id on reviewers' repositores. If `to = "a"`, the function places the file into each of the folders containing the reviewers' files on authors' repositories.
#' @param file Character. File name of file to be added.
#' @param dblind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `dblind = TRUE`, reviewer folders are identified by the reviewer's ID. If `dblind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_add_file(org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' to = "r",
#' file = "rfeedback_hw2_blank.Rmd",
#' prefix = "hw2-",
#' dblind = TRUE)
#' }
#'
#' @export
peer_add_file = function(org,
                         roster,
                         to = c("r", "a"),
                         file,
                         dblind = FALSE,
                         prefix = "",
                         suffix = "",
                         message = NULL,
                         branch = "master",
                         overwrite = FALSE) {
  arg_is_chr_scalar(to)
  stopifnot(to %in% c("r", "a"))

  rdf = peer_read_roster(roster)$rdf
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  purrr::walk2(author, author_random,
               function(author, author_random) {

                 reviewer = peer_get_reviewer(author, rdf, anonym = FALSE)
                 reviewer_no = paste0("r", seq_len(length(reviewer)))

                 purrr::walk2(reviewer, reviewer_no,
                              function(reviewer, reviewer_no) {
                                if (to == "r") {
                                  repo = as.character(glue::glue("{org}/{prefix}{reviewer}{suffix}"))
                                  folder = author_random
                                } else {
                                  repo = as.character(glue::glue("{org}/{prefix}{author}{suffix}"))
                                  if (!dblind) {
                                    folder = reviewer
                                  } else {
                                    folder = reviewer_no
                                  }
                                }

                                # repo_add_file does modification check
                                repo_add_file(
                                  repo = repo,
                                  file = file,
                                  folder = folder,
                                  preserve_path = FALSE,
                                  overwrite = overwrite
                                )
                              })
               })
}




#' Collect scores from reviewer feedback forms
#'
#' The `peer_score()` function collects score information from the YAML of a feedback form within a student's repository. It outputs a new .csv file, with rows specifying individual question scores for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_rform`.
#' @param from Character. Specifies whether scores are to be collected from reviewer (`r`) or author (`a`) repositories.
#' @param file Character. File name of feedback form (must be .Rmd document).
#' @param dblind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `dblind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `dblind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_score(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' from = "a",
#' file = "afeedback_blank.Rmd",
#' dblind = T,
#' prefix = "hw2-)
#' }
#'
#' @export
#'
peer_score = function(org,
                      roster,
                      from = c("r", "a"),
                      file,
                      dblind = FALSE,
                      prefix = "",
                      suffix = "",
                      write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, file)
  arg_is_chr_scalar(from)
  stopifnot(from %in% c("r", "a"))

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", file)) {
    usethis::ui_stop("{usethis::ui_field('file')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  # Read and check roster file
  temp = peer_read_roster(roster,
                          fname_append = glue::glue("{from}scores"),
                          prefix = prefix,
                          suffix = suffix)
  rdf = temp$rdf
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  out = purrr::map2_dfr(author, author_random,
                        function(author, author_random) {

                          # Grab reviewers
                          reviewer = peer_get_reviewer(author, rdf, anonym = FALSE)
                          reviewer_random = peer_get_reviewer(author, rdf, anonym = TRUE)
                          reviewer_no = paste0("r", seq_len(length(reviewer)))

                          purrr::pmap_dfr(list(reviewer, reviewer_no),
                                          function(reviewer,
                                                   reviewer_no) {
                                            if (from == "r") {
                                              repo = glue::glue("{org}/{prefix}{reviewer}{suffix}")
                                              ghpath = glue::glue("{author_random}/{file}")
                                              tag = "q"
                                              user = author
                                              r_no = reviewer_no
                                            } else {
                                              repo = glue::glue("{org}/{prefix}{author}{suffix}")
                                              tag = "c"
                                              user = reviewer
                                              r_no = paste0("r", which(peer_get_reviewer(user, rdf) == author))
                                              if (dblind) {
                                                ghpath = glue::glue("{reviewer_no}/{file}")
                                              } else {
                                                ghpath = glue::glue("{reviewer}/{file}")
                                              }
                                            }

                                            feedback = purrr::safely(get_file)(repo, ghpath)
                                            if (succeeded(feedback)) {

                                              tc = textConnection(feedback$result)
                                              scores = rmarkdown::yaml_front_matter(tc)$params
                                              scores[scores == "NA"] <- NA

                                              setNames(c(user, r_no, scores),
                                                       c("user", "r_no", paste0(tag, 1:length(scores))))

                                            } else {
                                              usethis::ui_oops(
                                                "Cannot locate file {usethis::ui_value(ghpath)} on repo {usethis::ui_value(repo)}."
                                              )
                                            }
                                          })

                        }) %>%
    # Getting data frame in right format
    tidyr::gather(q_name, q_value, -user, -r_no) %>%
    tidyr::unite("new", c("r_no", "q_name")) %>%
    tidyr::spread(new, q_value) %>%
    merge(rdf, all.y = T)

  out = out[, union(names(rdf), names(out))]

  if (write_csv) {
    readr::write_csv(out, temp$fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    out
  }
}


#' Return peer feedback to authors
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#' @param file Character. File name or vector of file names to be included.
#' @param dblind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `dblind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `dblind = FALSE`, reviewer folders are identified by the original user names. Defaults to `FALSE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message, defaults to "Assigning review."
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_return(org = "ghclass-test,
#' roster = "hw2_roster_seed12345.csv",
#' file = c("hw2_task.Rmd", "rfeedback_blank.Rmd"),
#' prefix = "hw2-,
#' dblind = T)
#' }
#'
#' @export
#'
peer_return = function(org,
                       roster,
                       file,
                       dblind = FALSE,
                       prefix = "",
                       suffix = "",
                       message = "Returning review",
                       branch = "master",
                       overwrite = FALSE) {

  arg_is_chr(org, file, prefix, suffix, branch)
  arg_is_chr(message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  rdf = peer_read_roster(roster)$rdf
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  purrr::walk2(author, author_random,
               function(author, author_random){

                 repo_a = glue::glue("{org}/{prefix}{author}{suffix}")
                 ghpath_r = glue::glue("{author_random}/{file}")

                 # reviewer-specific elements
                 reviewer = peer_get_reviewer(author, rdf, anonym = FALSE)
                 reviewer_no = paste0("r", seq_len(length(reviewer)))
                 repo_r = glue::glue("{org}/{prefix}{reviewer}{suffix}")

                 purrr::pwalk(list(reviewer, reviewer_no, repo_r),
                             function(reviewer, reviewer_no, repo_r){

                               if (!dblind) {
                                 ghpath_a = glue::glue("{reviewer}/{file}")
                               } else {
                                 ghpath_a = glue::glue("{reviewer_no}/{file}")
                               }

                               content = purrr::map(ghpath_r,
                                                    function(ghpath_r) {
                                                      if (file_exists(repo = repo_r, file = ghpath_r)) {
                                                        res = purrr::safely(get_file)(repo = repo_r,
                                                                                      file = ghpath_r,
                                                                                      branch = branch)
                                                        if (succeeded(res)) {
                                                          res$result
                                                        }
                                                      }
                                                    })

                               purrr::walk2(content, ghpath_a,
                                            function(content, ghpath_a){
                                              if (!file_exists(repo_a, ghpath_a, verbose = FALSE) |
                                                  overwrite == TRUE) {
                                                if (!is.null(content)) {
                                                  put_file(
                                                    repo = repo_a,
                                                    path = ghpath_a,
                                                    content = content,
                                                    message = message,
                                                    branch = branch,
                                                    verbose = TRUE
                                                  )
                                                }
                                              } else {
                                                usethis::ui_oops(
                                                  "Failed to add {usethis::ui_value(ghpath_a)} to {usethis::ui_value(repo_a)}: already exists."
                                                )
                                              }
                                            })
                             })
                 })
}
