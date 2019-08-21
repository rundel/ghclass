format_rev = function(prefix, suffix) {
  tag = "review"
  if (prefix != "" & suffix == "") {
    list(prefix_review = paste0(prefix, tag, "-"),
         suffix_review = suffix)
  } else {
    list(prefix_review = prefix,
         suffix_review = paste0(suffix, "-", tag))
  }
}


# Helper function for Latin square
latin_square = function(j, n) {
  i <- seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}

# Reads roster file
peer_roster_process = function(roster) {
  is_df = is.data.frame(roster)
  is_chr = is.character(roster)

  if (is_df) {
    tmp = tibble::as_tibble(purrr::modify_if(roster, is.factor, as.character))
  } else if (is_chr) {
    if (fs::file_exists(roster)) {
      res = suppressMessages(purrr::safely(readr::read_csv)(roster))
      status_msg(res,
                 fail = "Cannot read {usethis::ui_field('roster')}.")
      if (succeeded(res))
        tmp = res[["result"]]
    } else {
      usethis::ui_stop("Cannot locate file: {usethis::ui_value(roster)}")
    }
  } else {
    usethis::ui_stop("{usethis::ui_field('roster')} must be a data.frame or .csv file.")
  }

  rdf = peer_roster_check(tmp)
  if (!is.null(rdf)) {
    rdf
  } else {
    usethis::ui_stop(
      "Please supply a peer review roster in the expected format (see reference for `peer_roster_create()`)."
    )
  }
}

# Checks whether necessary column names are present
peer_roster_check = function(roster) {
  # Check user and user_random colnames
  val = c("user", "user_random")
  passed1 = purrr::map_lgl(val,
                           function(val) {
                             if (!(val %in% names(roster))) {
                               usethis::ui_oops("{usethis::ui_field('roster')} must contain column {usethis::ui_value(val)}")
                               FALSE
                             } else {
                               TRUE
                             }
                           })

  # Check rev* colname(s)
  val2 = "rev*"
  passed2 = TRUE
  if (!(any(grepl("^rev[0-9]+$", names(roster))))) {
    usethis::ui_oops(
      "{usethis::ui_field('roster')} must contain at least one column {usethis::ui_value(val2)}, where '*' denotes a reviewer number."
    )
    passed2 = FALSE
  }

  if (!all(c(passed1, passed2))) {
    NULL
  } else {
    roster
  }
}

# Expand roster to make mapping over repos easier
peer_roster_expand = function(org,
                              roster,
                              prefix = "",
                              suffix = "",
                              prefix_review = "",
                              suffix_review = "") {
  arg_is_chr_scalar(org, prefix, suffix, prefix_review, suffix_review)

  rdf = peer_roster_process(roster)

  out = peer_roster_format_cols(rdf, prefix, suffix, prefix_review, suffix_review)

  out
}

peer_roster_format_cols = function(roster, prefix, suffix, prefix_review, suffix_review) {
  purrr::map_df(roster[["user"]],
    ~ tibble::tibble(
      aut = .x,
      aut_random = as.character(roster[["user_random"]][roster[["user"]] == .x]),
      repo_aut = paste0(org, "/", prefix, .x, suffix),
      rev = peer_get_rev(.x, roster, "rev"),
      rev_random = peer_get_rev(.x, roster, "rev_random"),
      rev_no = peer_get_rev(.x, roster, "rev_no"),
      repo_rev = paste0(org, "/", prefix, rev, suffix),
      repo_rev_review = paste0(org, "/", prefix_review, rev, suffix_review),
    )
  )
}


# Grab reviewer for author
peer_get_rev = function(aut,
                        roster,
                        out = c("rev",
                                "rev_random",
                                "rev_no")) {
  m = seq_len(length(names(roster)[grepl("^rev[0-9]+$", names(roster))]))
  rev_random = as.character(roster[roster[["user"]] == aut, paste0("rev", m)])
  rev = roster[["user"]][purrr::map_int(rev_random, ~ which(roster[["user_random"]] == .x))]
  rev_no = names(roster)[purrr::map_int(rev_random, ~ which(roster[roster[["user"]] == aut,] == .x))]

  if (out == "rev") {
    rev
  } else if (out == "rev_random") {
    rev_random
  } else if (out == "rev_no") {
    rev_no
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

peer_issue_label_create = function(repo) {
  arg_is_chr(repo)

  name = c(":pencil: Complete review", ":mag: Inspect review")
  color = c("ff004d", "ffb400")
  desc = c("Please complete review", "Please inspect and rate review")

  purrr::pmap_dfr(list(name, color, desc),
                  function(name, color, desc) {
                    res = purrr::safely(github_api_label_create)(
                      repo = repo,
                      name = name,
                      color = color,
                      description = desc
                    )

                    if (succeeded(res)) {
                      created = TRUE
                      error = NA
                    } else {
                      created = FALSE
                      error = res[["error"]][["headers"]][["status"]]
                    }
                    tibble::tibble(
                      repo = repo,
                      label = name,
                      created = created,
                      error = error
                    )
                  })
}


peer_issue_label_apply = function(org,
                                  repo = NULL,
                                  filter = NULL,
                                  exclude = FALSE,
                                  verbose = FALSE,
                                  show_label_result = FALSE) {
  arg_is_chr_scalar(org)
  arg_is_chr_scalar(filter, allow_null = TRUE)
  arg_is_chr(repo, allow_null = TRUE)
  arg_is_lgl(exclude, verbose, show_label_result)

  if (is.null(repo)) {
    repo = org_repos(
      org = org,
      filter = filter,
      exclude = exclude,
      full_repo = TRUE
    )
  }

  usethis::ui_info("Applying labels: This might take a moment...")
  out = purrr::map_dfr(repo, ~ peer_issue_label_create(.x))

  # TODO: Improve messaging about label creation
  # Note that the 422 error likely means that the label already exists.
  if (verbose)
    peer_issue_label_apply_msg(out)

  if (show_label_result)
    out

}

peer_issue_label_apply_msg = function(label_df) {
  purrr::walk(unique(label_df[["label"]]),
              function(x) {
                repo_label_yes = label_df[["repo"]][label_df[["label"]] == x &
                                                      label_df[["created"]]]
                repo_label_no = label_df[["repo"]][label_df[["label"]] == x &
                                                     !label_df[["created"]]]

                if (length(repo_label_yes) > 0)
                  usethis::ui_done(
                    "Created label {usethis::ui_value(x)} for {usethis::ui_value(length(repo_label_yes))} repositories."
                  )

                if (length(repo_label_no) > 0)
                  usethis::ui_oops(
                    "Failed to create label {usethis::ui_value(x)} for {usethis::ui_value(length(repo_label_no))} repositories. The label may already exist. Re-run with `show_label_result = TRUE` for more information."
                  )

              })
}

peer_issue_create = function(out,
                             title,
                             step = c("review", "rating"),
                             org,
                             prefix,
                             suffix,
                             branch = "master") {
  arg_is_chr_scalar(step, prefix, suffix, org, branch, title)

  if (is.null(out[['repo']])) {
    usethis::ui_oops("Skipping issue creation: no files found for any repositories.")
  }

  purrr::walk(unique(out[['repo']]),
              function(r) {
                sub = out[out[['repo']] == r,]

                url_start = list(
                  blob = paste0("https://github.com/", r, "/blob/", branch, "/"),
                  tree = paste0("https://github.com/", r, "/tree/", branch, "/"),
                  commit = paste0("https://github.com/", r, "/commit/")
                )

                if (step == "review") {
                  body = peer_issue_body_review(sub = sub, url_start = url_start)
                  labels = list(":pencil: Complete review")
                } else if (step == "rating") {
                  body = peer_issue_body_rating(sub = sub, url_start = url_start)
                  labels = list(":mag: Inspect review")
                }

                assignee = peer_repo_get_user(
                  repo = r,
                  org = org,
                  prefix = prefix,
                  suffix = suffix
                )

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


peer_issue_body_review = function(sub, url_start) {
  aut = unique(sub[['target_folder']])

  rev_txt = purrr::map_chr(aut,
                           function(y) {
                             paste(
                               glue::glue("**For {y}**"),
                               issue_txt_assignment(
                                 sub = sub,
                                 aut = y,
                                 url_start = url_start
                               ),
                               issue_txt_complete_review(
                                 sub = sub,
                                 aut = y,
                                 url_start = url_start
                               ),
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

issue_txt_assignment = function(sub, aut, url_start) {
  tmp = sub[sub[['category']] == "assignment" &
              sub[['target_folder']] == aut, ]

  if (nrow(tmp) > 0) {
    url = glue::glue("{url_start[['tree']]}{aut}")
    glue::glue("- [ ] Review [assignment file(s)]({url}).")
  }
}

issue_txt_complete_review = function(sub, aut, url_start) {
  tmp = sub[sub[['category']] == "review" &
              sub[['target_folder']] == aut, ]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start[['blob']]}{tmp[['path']]}")
    path_txt = sub(glue::glue("{aut}/"), "", tmp[['path']])
    glue::glue("- [ ] Fill out review form: [{path_txt}]({url}).")
  }
}


peer_issue_body_rating = function(sub, url_start = url_start) {
  rev = unique(sub[['target_folder']])

  rev_txt = purrr::map_chr(rev,
                           function(y) {
                             paste(
                               glue::glue("**From {y}**"),
                               issue_txt_diff(
                                 sub = sub,
                                 rev = y,
                                 url_start = url_start
                               ),
                               issue_txt_read_review(
                                 sub = sub,
                                 rev = y,
                                 url_start = url_start
                               ),
                               issue_txt_complete_rating(
                                 sub = sub,
                                 rev = y,
                                 url_start = url_start
                               ),
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

issue_txt_diff = function(sub, rev, url_start = url_start) {
  tmp = sub[sub[['category']] == "assignment" &
              sub[['target_folder']] == rev, ]

  diff_txt = purrr::map_chr(seq_len(nrow(tmp)),
                            function(z) {
                              path_txt = sub(glue::glue("{rev}/"), "", tmp[['path']][z])
                              if (!is.na(tmp[['commit_sha']][z])) {
                                commit_url = paste0(url_start[['commit']], tmp[['commit_sha']][z])
                                glue::glue("- [ ] Review changes suggested for [{path_txt}]({commit_url}).")
                              } else {
                                blob_url = paste0(url_start[['blob']], tmp[['path']][z])
                                glue::glue("- [ ] No direct edits were made to [{path_txt}]({blob_url}).")
                              }
                            })
  paste(diff_txt, collapse = "\n")
}

issue_txt_read_review = function(sub, rev, url_start = url_start) {
  tmp = sub[sub[['category']] == "review" &
              sub[['target_folder']] == rev, ]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start[['blob']]}{tmp[['path']]}")
    path_txt = sub(glue::glue("{rev}/"), "", tmp[['path']])
    glue::glue("- [ ] Read review: [{path_txt}]({url}).")
  }
}


issue_txt_complete_rating = function(sub, rev, url_start = url_start) {
  tmp = sub[sub[['category']] == "rating" &
              sub[['target_folder']] == rev, ]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start[['blob']]}{tmp[['path']]}")
    path_txt = sub(glue::glue("{rev}/"), "", tmp[['path']])
    glue::glue("- [ ] Fill out rating form: [{path_txt}]({url}).")
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

  purrr::map(local_path,
             function(local_path) {
               if (!is.null(local_path)) {
                 file_status = fs::file_exists(local_path)
                 if (!file_status)
                   usethis::ui_stop("Unable to locate the following file: {usethis::ui_value(local_path)}")

                 if (check_rmd &
                     (tolower(fs::path_ext(local_path)) == "rmd")) {
                   list(
                     content = readChar(local_path, file.info(local_path)[['size']]),
                     path = fs::path_file(local_path)
                   )
                 } else {
                   usethis::ui_stop(
                     "{usethis::ui_field('local_path')} must be a {usethis::ui_path('.Rmd')} file."
                   )
                 }
               }
             })
}


# subset repository files
repo_files_select = function(repo,
                             repo_files = NULL,
                             exclude_pattern,
                             branch) {
  arg_is_chr_scalar(repo, branch)
  arg_is_chr(exclude_pattern, allow_null = T)

  if (is.null(repo_files)) {
    repo_files = repo_files(repo = repo, branch = branch)
  }

  path = repo_files[['path']][repo_files[['type']] == "blob" &
                                !grepl("/", repo_files[['path']])]

  rx_exclude_pattern = paste(utils::glob2rx(exclude_pattern), collapse = "|")
  path[!grepl(rx_exclude_pattern, path)]
}

format_commit_output = function(res = NULL,
                                target_files = NULL,
                                target_repo,
                                target_path,
                                target_folder,
                                category,
                                changed = NA) {
  arg_is_chr_scalar(target_repo, target_path, target_folder, category)
  arg_is_lgl_scalar(changed, allow_na = TRUE)

  if (is.null(res) & !is.null(target_files)) {
    mode = target_files[["mode"]]
    type = target_files[["type"]]
    sha = target_files[["sha"]]
    size = target_files[["size"]]
    url = target_files[["url"]]
    added = FALSE
    commit_sha = NA
  } else if (!is.null(res) & succeeded(res)) {
    # https://developer.github.com/v3/git/trees/#create-a-tree
    # mode = 100644 & type = "blob" is added manually to reduce API calls
    # Needed to accurately track files present on repositories
    # Assumes that only file blobs are added to repositories
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

  if (!is.na(changed)) {
    changed = changed
  }

  tibble::tibble(
    repo = target_repo,
    path = target_path,
    mode = as.numeric(mode),
    type = type,
    sha = sha,
    size = size,
    url = url,
    target_folder = target_folder,
    changed = changed,
    added = added,
    commit_sha = commit_sha,
    category = category
  )
}

peer_add_content = function(target_repo,
                            target_folder,
                            target_files,
                            content,
                            content_compare = NULL,
                            category,
                            message,
                            branch,
                            overwrite) {
  arg_is_chr(target_repo)
  arg_is_chr_scalar(category, target_folder)
  arg_is_chr_scalar(message, branch, allow_null = TRUE)
  arg_is_lgl(overwrite)

  # TODO: Review to simplify nested mapping
  out = purrr::map_dfr(target_repo,
                       function(r) {
                         sub_r = target_files[target_files[['repo']] == r,]

                         purrr::map_dfr(content,
                                        function(c) {
                                          changed = NA

                                          target_path = paste0(target_folder, "/", c[['path']])

                                          target_exists = target_path %in% sub_r[['path']]

                                          if (target_exists &
                                              !overwrite) {
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

                                            # Compare with content_compare
                                            if (!is.null(content_compare)) {
                                              content_compare_path = purrr::map_chr(content_compare, "path")
                                              if (length(content_compare_path) > 0) {
                                                n = which(purrr::map_chr(content_compare, "path") == c[['path']])
                                                changed = !isTRUE(all.equal(c[['content']][1],
                                                                            content_compare[[n]][['content']][1]))
                                              }
                                            }

                                            if (is.na(changed) |
                                                changed) {
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
                                                category = category,
                                                changed = changed
                                              )
                                            } else {
                                              format_commit_output(
                                                target_files = sub_r[sub_r[['path']] == target_path, ],
                                                target_repo = r,
                                                target_path = target_path,
                                                target_folder = target_folder,
                                                category = category,
                                                changed = changed
                                              )
                                            }
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
repo_path_content_grab = function(repo,
                                  path,
                                  repo_files = NULL,
                                  branch) {
  arg_is_chr_scalar(repo, branch)
  arg_is_chr(path)

  # TODO: Review to simplify nested mapping
  purrr::map(path,
             function(path) {
               if (is.null(repo_files)) {
                 repo_files = repo_files(repo = repo, branch = branch)
               }

               path_exists = path %in% repo_files[['path']]

               if (path_exists) {
                 res = purrr::safely(repo_get_file)(repo = repo,
                                                    file = path,
                                                    branch = branch)
                 if (succeeded(res))
                   list(content = res[['result']],
                        path = path)
               } else {
                 list(content = NULL,
                      path = NULL)
               }
             })
}

content_path_folder_strip = function(list, folder) {
  purrr::map(list,
             ~ purrr::modify_at(.,
                                .at = c("path"),
                                ~ gsub(paste0(folder, "/"), "", .x)))
}
