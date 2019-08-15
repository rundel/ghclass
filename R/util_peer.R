# Helper function for Latin square
latin_square = function(j, n) {
  i <- seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}

# Reads roster file
peer_roster_read = function(roster) {
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
peer_roster_check = function(roster) {
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

# Expand roster to make mapping over repos easier
peer_roster_expand = function(org,
                              roster,
                              prefix = "",
                              suffix = "",
                              prefix_rev = "",
                              suffix_rev = "") {
  arg_is_chr_scalar(org, prefix, suffix, prefix_rev, suffix_rev)

  rdf = peer_roster_read(roster)
  peer_roster_check(rdf)

  out = purrr::map_dfr(rdf[['user']],
                       function(y) {
                         tibble::tibble(
                           author = y,
                           author_random = as.character(rdf[rdf$user == y, 'user_random']),
                           repo_a = as.character(glue::glue("{org}/{prefix}{y}{suffix}")),
                           reviewer = peer_get_reviewer(y, rdf, "reviewer"),
                           reviewer_random = peer_get_reviewer(y, rdf, "reviewer_random"),
                           reviewer_no = peer_get_reviewer(y, rdf, "reviewer_no"),
                           repo_r = as.character(glue::glue("{org}/{prefix}{reviewer}{suffix}")),
                           repo_r_rev = as.character(glue::glue(
                             "{org}/{prefix_rev}{reviewer}{suffix_rev}"
                           ))
                         )
                       })
  out
}

# Grab reviewer for author
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



github_api_issue_create = function(repo, title, body, assignee, labels) {
  gh::gh(
    "POST /repos/:owner/:repo/issues",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    title = title,
    body = body,
    assignee = assignee,
    labels = labels,
    .token = github_get_token()
  )
}

github_api_label_create = function(repo, name, color, description) {
  gh::gh(
    "POST /repos/:owner/:repo/labels",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    name = name,
    color = color,
    description = description,
    .token = github_get_token()
  )
}

peer_issue_label_create = function(repo, verbose = FALSE) {
  arg_is_chr(repo)

  name = c(":pencil: Complete review", ":mag: Inspect review")
  color = c("ff004d", "ffb400")
  desc = c("Please complete review", "Please inspect and rate review")

  purrr::pwalk(list(name, color, desc),
               function(name, color, desc) {
                 res = purrr::safely(github_api_label_create)(
                   repo = repo,
                   name = name,
                   color = color,
                   description = desc
                 )

                 if (verbose) {
                   status_msg(
                     res,
                     glue::glue("Created label {usethis::ui_value(name)}"),
                     glue::glue("Failed to create label {usethis::ui_value(name)}")
                   )
                 }
               })
}


peer_issue_label_apply = function(org,
                                  filter = NULL,
                                  exclude = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_lgl(exclude)

  repos = org_repos(
    org = org,
    filter = filter,
    exclude = exclude,
    full_repo = TRUE
  )

  usethis::ui_info("Applying labels: This might take a moment...")
  purrr::walk(repos, ~ peer_issue_label_create(.))
  usethis::ui_done("Applied peer review labels to all repositories in {usethis::ui_value(org)}.")

}

peer_issue_create = function(out,
                             title = "Assigning review",
                             type = c("review", "rating"),
                             org,
                             prefix,
                             suffix,
                             branch = "master") {
  arg_is_chr_scalar(type, prefix, suffix, org, branch, title)

  if (is.null(out[['repo']])) {
    usethis::ui_oops("Skipping issue creation: no files found for any repositories.")
  }

  purrr::walk(unique(out[['repo']]),
              function(r) {
                sub = out[out[['repo']] == r,]

                url_start_blob = paste0("https://github.com/", r, "/blob/", branch, "/")
                url_start_tree = paste0("https://github.com/", r, "/tree/", branch, "/")
                url_start_commit = paste0("https://github.com/", r, "/commit/")

                if (type == "review") {
                  body = peer_issue_body_review(sub)
                  assignee = peer_repo_get_user(
                    repo = r,
                    org = org,
                    prefix = prefix,
                    suffix = suffix
                  )
                  labels = list(":pencil: Complete review")
                } else if (type == "rating") {
                  body = peer_issue_body_rating(sub)
                  assignee = unique(sub[['author']])
                  labels = list(":mag: Inspect review")
                }

                res = purrr::safely(github_api_issue_create)(
                  repo = r,
                  title = title,
                  body = body,
                  assignee = assignee,
                  labels = labels
                )

                status_msg(res,
                           glue::glue("Posted issue for {r}"),
                           glue::glue("Cannot post issue for {r}"))

              })
}

peer_issue_create_rating = function(out,
                                    title = "Reviewer feedback") {
  purrr::walk(unique(out[['repo']]),
              function(x) {
                sub = out[out[['repo']] == x, ]

                url_start_blob = paste0("https://github.com/", x, "/blob/master/")
                url_start_commit = paste0("https://github.com/", x, "/commit/")

                res = purrr::safely(github_api_issue_create)(
                  repo = unique(sub[['repo']]),
                  title = title,
                  body = peer_issue_body_rating(sub),
                  assignee = unique(sub[['author']]),
                  labels = list(":mag: Inspect review")
                )

                status_msg(res,
                           glue::glue("Posted issue for {x}"),
                           glue::glue("Cannot post issue for {x}"))

              })
}


peer_issue_body_review = function(sub) {
  aut = unique(sub[['target_folder']])

  rev_txt = purrr::map_chr(aut,
                           function(y) {
                             paste(
                               glue::glue("**For {y}**"),
                               issue_txt_assignment(sub = sub, aut = y),
                               issue_txt_complete_review(sub = sub, aut = y),
                               sep = "\n"
                             )
                           })

  glue::glue(
    "Your peers' assignments have been added to this review repository.\n\n",
    "To start the review process, please clone the review repository as a new Version Control Project in RStudio to your local machine. Don't forget to commit and push your work when you are done.\n\n",
    'After you cloned the review repository, please complete the following tasks for each of the authors:\n\n',
    paste(rev_txt, collapse = "\n\n"),
    "\n\n",
    "You may use the check boxes to keep track of your progress."
  )
}

issue_txt_assignment = function(sub, aut) {
  tmp = sub[sub[['category']] == "assignment" &
              sub[['target_folder']] == aut, ]

  if (nrow(tmp) > 0) {
    url = glue::glue("{url_start_tree}{aut}")
    glue::glue("- [ ] Review [assignment file(s)]({url}).")
  }
}

issue_txt_complete_review = function(sub, aut) {
  tmp = sub[sub[['category']] == "review" &
              sub[['target_folder']] == aut, ]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start_blob}{tmp[['path']]}")
    path_txt = sub(glue::glue("{aut}/"), "", tmp[['path']])
    glue::glue("- [ ] Fill out review form: [{path_txt}]({url}).")
  }
}


peer_issue_body_rating = function(sub) {
  rev = unique(sub[['target_folder']])

  rev_txt = purrr::map_chr(rev,
                           function(y) {
                             paste(
                               glue::glue("**From {y}**"),
                               issue_txt_diff(sub = sub, rev = y),
                               issue_txt_read_review(sub = sub, rev = y),
                               issue_txt_complete_rating(sub = sub, rev = y),
                               sep = "\n"
                             )
                           })

  glue::glue(
    "The feedback from your peers has been added to your repository.\n\n",
    "To finish the assignment, please pull changes into your assignment repository and complete the tasks below. Don't forget to commit and push your work when you are done.\n\n",
    paste(rev_txt, collapse = "\n\n"),
    "\n\n\n",
    "*You may use the check boxes to keep track of your progress*."
  )

}

issue_txt_diff = function(sub, rev) {
  tmp = sub[sub[['type']] == "assignment" &
              sub[['target_folder']] == rev, ]

  diff_txt = purrr::map_chr(seq_len(nrow(tmp)),
                            function(z) {
                              if (!is.na(tmp[['commit_sha']][z])) {
                                commit_url = paste0(url_start_commit, tmp[['commit_sha']][z])
                                glue::glue("- [ ] Review changes suggested for [{tmp[['path']][z]}]({commit_url}).")
                              } else {
                                blob_url = paste0(url_start_blob, tmp[['path']][z])
                                glue::glue("- [ ] No direct edits were made to [{tmp[['path']][z]}]({blob_url}).")
                              }
                            })
  paste(diff_txt, collapse = "\n")
}

issue_txt_read_review = function(sub, rev) {
  tmp = sub[sub[['type']] == "review" &
              sub[['added']] & sub[['target_folder']] == rev, ]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start_blob}{tmp[['target_folder']]}/{tmp[['path']]}")
    glue::glue("- [ ] Read review: [{tmp[['path']]}]({url}).")
  }
}


issue_txt_complete_rating = function(sub, rev) {
  temp = sub[sub[['type']] == "rating" &
               sub[['added']] & sub[['target_folder']] == rev, ]

  if (nrow(temp) > 0) {
    arg_is_chr_scalar(temp[['path']])
    url = glue::glue("{url_start_blob}{temp[['target_folder']]}/{temp[['path']]}")
    glue::glue("- [ ] Fill out rating form: [{temp[['path']]}]({url}).")
  }
}


format_folder = function(folder, path) {
  if (!is.null(folder)) {
    paste0(folder, "/", path)
  } else {
    path
  }
}


peer_file_place = function(repo_files,
                           target_repo,
                           input,
                           message,
                           branch,
                           verbose,
                           overwrite) {
  purrr::walk(input,
              function(y) {
                gh_path = glue::glue("{y[[1]]}/{fs::path_file(y[[2]])}")

                if (!(gh_path %in% repo_files[['path']]) |
                    overwrite) {
                  if ((gh_path %in% repo_files[['path']]) & overwrite) {
                    sha = repo_files[['sha']][repo_files[['path']] == gh_path]
                  } else {
                    sha = NULL
                  }

                  peer_repo_put_file(
                    repo = target_repo,
                    path = gh_path,
                    content = read_bin_file(y[[2]]),
                    message = message,
                    branch = branch,
                    verbose = verbose,
                    sha = sha
                  )
                } else {
                  usethis::ui_oops(
                    paste(
                      'Failed to add {usethis::ui_value(gh_path)} to {usethis::ui_value(target_repo)}: already exists.',
                      'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
                    )
                  )
                }
              })
}


local_path_content_grab = function(local_path = NULL,
                                   check_rmd = TRUE) {

  arg_is_chr(local_path, allow_null = TRUE)
  arg_is_lgl_scalar(check_rmd)

  out = purrr::map(local_path,
                   function(local_path){

                     if (!is.null(local_path)) {
                       file_status = fs::file_exists(local_path)

                       if (file_status) {
                         if (check_rmd) {
                           if (!grepl("\\.[rR]md$", fs::path_file(local_path))) {
                             usethis::ui_stop("{usethis::ui_field('local_path')} must be a {usethis::ui_path('.Rmd')} file.")
                           }
                         }

                         list(content = read_bin_file(local_path),
                              path = fs::path_file(local_path))

                       } else {
                         usethis::ui_stop("Unable to locate the following file: {usethis::ui_value(local_path)}")
                       }
                     }
                   })
  out
}

# Currently works for 1 folder & 1 path, but vectorized over repos
peer_add_local = function(target_repo,
                          target_folder,
                          target_files,
                          content,
                          path,
                          category = c("review", "rating"),
                          message,
                          branch,
                          overwrite) {
  arg_is_chr(target_repo)
  arg_is_chr_scalar(target_folder, path, message, branch, category)
  arg_is_raw(content)
  arg_is_lgl(overwrite)

  target_path = paste0(target_folder, "/", path)

  out = purrr::map_dfr(target_repo,
                       function(r) {

                         sub = target_files[target_files[['repo']] == r, ]
                         target_exists = target_path %in% sub[['path']]

                         if (target_exists & !overwrite) {
                           usethis::ui_oops(
                             paste(
                               'Failed to add {usethis::ui_value(target_path)} to {usethis::ui_value(r)}: already exists.',
                               'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
                             )
                           )

                           format_commit_output(
                             target_files = sub[sub[['path']] == target_path, ],
                             target_repo = r,
                             target_path = target_path,
                             target_folder = target_folder,
                             category = category
                           )

                         } else {
                           if (target_exists) {
                             sha = sub[['sha']][sub[['path']] == target_path]
                           } else {
                             sha = NULL
                           }

                           res = peer_repo_put_file(
                             repo = r,
                             path = target_path,
                             content = content,
                             message = message,
                             branch = branch,
                             sha = sha
                           )

                           format_commit_output(
                             res = res,
                             target_repo = r,
                             target_path = target_path,
                             target_folder = target_folder,
                             category = category
                           )
                         }
                       })

  out
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
peer_mirror_review = function(content_og,
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

  if (length(content_og) != length(path)) {
    usethis::ui_oops("{content_og} must be of same length as {path}.")
  }

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
                                 !is.null(content_og[p]) &
                                 isTRUE(all.equal(content_og[p][[1]][1], res$result[[1]][1]))) {
                               usethis::ui_oops(
                                 "Skipping mirroring {usethis::ui_value(gh_path[p])} from {usethis::ui_value(source_repo)} to {usethis::ui_value(target_repo)}: no changes detected."
                               )
                               changed = FALSE
                               added = FALSE
                               commit_sha = NA
                               type = "assignment"
                             } else {
                               if (!autfile[p]) {
                                 changed = NA
                                 type = "review"
                               } else {
                                 changed = TRUE
                                 type = "assignment"
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
                               path = gh_path[p],
                               target_folder = target_folder,
                               changed = changed,
                               added = added,
                               sha = ifelse(is.null(sha), NA, sha),
                               commit_sha = commit_sha,
                               type = type
                             )

                           } else {
                             usethis::ui_oops(
                               "Failed to locate {usethis::ui_value(source_path)} on {usethis::ui_value(source_repo)}"
                             )
                           }
                         }
                       })
}





repo_files_select = function(repo, exclude_extension, branch) {
  files = repo_files(repo = repo, branch = branch)
  path = files[['path']][files[['type']] == "blob" &
                           !grepl("/", files[['path']])]
  exclude_extension = unlist(purrr::map_if(
    exclude_extension,
    grepl("^\\.", exclude_extension),
    ~ sub("^\\.", "", .x)
  ))
  path[!purrr::map_lgl(path,
                       ~ any(stringr::str_detect(
                         .x, glue::glue("\\.{exclude_extension}$")
                       )))]
}

format_commit_output = function(res = NULL,
                                target_files = NULL,
                                target_repo,
                                target_path,
                                target_folder,
                                category) {
  if (is.null(res) & !is.null(target_files)) {
    mode = target_files[["mode"]]
    type = target_files[["type"]]
    sha = target_files[["sha"]]
    size = target_files[["size"]]
    url = target_files[["url"]]
    added = FALSE
    commit_sha = NA
  } else if (!is.null(res) & succeeded(res)) {
    mode = 100644
    type = "blob"
    sha = res[["result"]][["content"]][["sha"]]
    size = res[["result"]][["content"]][["size"]]
    url = res[["result"]][["content"]][["git_url"]]
    added = TRUE
    commit_sha = res[["result"]][["commit"]][["sha"]]
  } else {
    mode = NA
    type = NA
    sha = NA
    size = NA
    url = NA
    added = FALSE
    commit_sha = NA
  }
  tibble::tibble(
    repo = target_repo,
    path = target_path,
    mode = mode,
    type = type,
    sha = sha,
    size = size,
    url = url,
    target_folder = target_folder,
    changed = NA,
    added = added,
    commit_sha = commit_sha,
    category = category
  )
}

peer_add_content = function(target_repo,
                            target_folder,
                            target_files,
                            content,
                            category = c("assignment"),
                            message,
                            branch,
                            overwrite) {

  out = purrr::map_dfr(target_repo,
                       function(r) {

                         sub_r = target_files[target_files[['repo']] == r,]

                         purrr::map_dfr(content,
                                        function(c) {
                                          # Note this is similar to peer_file_place
                                          # Difference is in reading local file vs. passing it in
                                          # also saving it to an object
                                          target_path = paste0(target_folder, "/", c[['path']])

                                          target_exists = target_path %in% sub_r[['path']]

                                          if (target_exists & !overwrite) {
                                            usethis::ui_oops(
                                              paste(
                                                'Failed to add {usethis::ui_value(target_path)} to {usethis::ui_value(r)}: already exists.',
                                                'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
                                              )
                                            )

                                            format_commit_output(
                                              target_files = sub_r[sub_r[['path']] == target_path, ],
                                              target_repo = r,
                                              target_path = target_path,
                                              target_folder = target_folder,
                                              category = category
                                            )

                                          } else {

                                            if (target_exists) {
                                              sha = sub_r[['sha']][sub_r[['path']] == target_path]
                                            } else {
                                              sha = NULL
                                            }

                                            res = peer_repo_put_file(
                                              repo = r,
                                              path = target_path,
                                              content = c[['content']][1],
                                              branch = branch,
                                              sha = sha
                                            )

                                            format_commit_output(
                                              res = res,
                                              target_repo = r,
                                              target_path = target_path,
                                              target_folder = target_folder,
                                              category = category
                                            )

                                          }
                                        })
                       })
  out

}

# Extract user from a repo of format org/repo
peer_repo_get_user = function(repo, org, prefix, suffix) {
  tmp = sub(glue::glue("{org}/{prefix}[(-review)]*"), "", repo)
  tmp = sub(glue::glue("{suffix}[(-review)]*"), "", tmp)
  tmp
}

# Grab content of specific files on a repo; return list w content and name
repo_file_content_grab = function(repo, path, branch) {
  out = purrr::map(path,
             function(path) {
               res = purrr::safely(repo_get_file)(repo = repo,
                                                  file = path,
                                                  branch = branch)
               if (succeeded(res))
                 list(content = res$result,
                      path = path)

             })

  out

}
