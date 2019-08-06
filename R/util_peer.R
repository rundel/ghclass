# Reads roster file
peer_read_roster = function(roster) {
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

peer_expand_roster = function(org,
                              roster,
                              prefix = "",
                              suffix = "",
                              prefix_rev = "",
                              suffix_rev = "") {
  arg_is_chr_scalar(org, prefix, suffix, prefix_rev, suffix_rev)

  rdf = peer_read_roster(roster)
  peer_check_roster(rdf)

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
                           )),
                           reviewer_no_scorea = names(rdf)[purrr::map_int(reviewer_random, ~
                                                                            which(rdf[rdf$user_random == .x, ] == author_random))]
                         )
                       })
  out
}


peer_get_reviewer = function(author,
                             roster,
                             out = c("reviewer",
                                     "reviewer_random",
                                     "reviewer_no")) {
  m = seq_len(length(names(roster)[grepl("^rev[0-9]+$", names(roster))]))
  reviewer_random = as.character(roster[roster$user == author, paste0("rev", m)])
  reviewer = roster$user[purrr::map_int(reviewer_random, ~ which(roster$user_random == .x))]
  reviewer_no = names(roster)[purrr::map_int(reviewer_random, ~ which(roster[roster$user == author, ] == .x))]

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

peer_create_label = function(repo, verbose = FALSE) {
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


peer_apply_label = function(org,
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
  purrr::walk(repos, ~ peer_create_label(.))
  usethis::ui_done("Applied peer review labels to all repositories in {usethis::ui_value(org)}.")

}


peer_create_issue_review = function(rdf,
                                    form_review,
                                    title = "Author files") {
  purrr::walk(unique(rdf[['reviewer']]),
              function(x) {
                sub = rdf[rdf[['reviewer']] == x, ]

                res = purrr::safely(github_api_issue_create)(
                  repo = unique(sub[['repo_r_rev']]),
                  title = title,
                  body = peer_issue_body_review(sub, path, form_review),
                  assignee = x,
                  labels = list(":pencil: Complete review")
                )

                status_msg(res,
                           glue::glue("Posted issue for {x}"),
                           glue::glue("Cannot post issue for {x}"))

              })
}


peer_issue_body_review = function(sub,
                                  path,
                                  form_review = NULL) {
  arg_is_chr(path, allow_null = TRUE)
  arg_is_chr_scalar(form_review, allow_null = T)

  repo_r_rev = unique(sub[['repo_r_rev']])
  url_start_blob = glue::glue("https://github.com/{repo_r_rev}/blob/master/")
  url_start_tree = glue::glue("https://github.com/{repo_r_rev}/tree/master/")

  fdf = repo_files(repo_r_rev)

  out = purrr::map_dfr(sub[['author_random']],
                       function(y) {
                         if (!is.null(form_review)) {
                           rtemp = fdf[['path']][grepl(y, fdf[['path']]) &
                                                   grepl(form_review, fdf[['path']])]
                         }

                         tibble::tibble(
                           author_random = y,
                           rfeed = ifelse(
                             !is.null(form_review),
                             paste0(url_start_blob, rtemp),
                             character()
                           ),
                           url = paste0(url_start_blob, y)
                         )
                       })

  rev_txt = purrr::map_chr(sub[['author_random']],
                           function(y) {
                             paste(
                               glue::glue("**For {y}**"),
                               glue::glue(
                                 "- [ ] Review [assignment]({out$url[out$author_random == y]})."
                               ),
                               # Get author folder url
                               check_rfeed_review(out, y),
                               sep = "\n"
                             )
                           })

  glue::glue(
    "Your peers' assignments have been added your repository for review.\n\n",
    'Please complete the following tasks for each of the authors:\n\n',
    paste(rev_txt, collapse = "\n\n")
  )
}


peer_create_issue_rating = function(rdf,
                                    path,
                                    form_review,
                                    form_rating,
                                    title = "Reviewer feedback",
                                    label = "test",
                                    double_blind = FALSE) {
  purrr::walk(unique(rdf[['author']]),
              function(x) {
                sub = rdf[rdf[['author']] == x,]

                res = purrr::safely(github_api_issue_create)(
                  repo = unique(sub[['repo_a']]),
                  title = title,
                  body = peer_issue_body_rating(sub, path, form_review, form_rating, double_blind),
                  assignee = x,
                  labels = list(":mag: Inspect review")
                )

                status_msg(res,
                           glue::glue("Posted issue for {x}"),
                           glue::glue("Cannot post issue for {x}"))

              })
}


peer_issue_body_rating = function(sub,
                                  path,
                                  form_review = NULL,
                                  form_rating = NULL,
                                  double_blind) {
  arg_is_chr(path)
  arg_is_chr_scalar(form_review, form_rating, allow_null = T)

  repo_a = unique(sub[['repo_a']])
  url_start = glue::glue("https://github.com/{repo_a}/blob/master/")

  fdf = repo_files(repo_a)

  if (!double_blind) {
    rev_sub = sub[['reviewer']]
  } else {
    rev_sub = sub[['reviewer_no']]
  }

  #https://github.com/ghclass-test/homework2-thereanders/blob/master/rev1/rfeedback_blank.Rmd

  out = purrr::map_dfr(rev_sub,
                       function(y) {
                         if (!is.null(form_rating)) {
                           atemp = fdf[['path']][grepl(y, fdf[['path']]) &
                                                   grepl(form_rating, fdf[['path']])]
                         }

                         if (!is.null(form_review)) {
                           rtemp = fdf[['path']][grepl(y, fdf[['path']]) &
                                                   grepl(form_review, fdf[['path']])]
                         }

                         cbind(
                           tibble::tibble(
                             reviewer = y,
                             afeed = ifelse(
                               !is.null(form_rating),
                               paste0(url_start, atemp),
                               character()
                             ),
                             rfeed = ifelse(
                               !is.null(form_review),
                               paste0(url_start, rtemp),
                               character()
                             )
                           ),
                           create_diff_url(repo_a, as.character(glue::glue("{y}/{path}")))
                         )
                       })

  rev_txt = purrr::map_chr(rev_sub,
                           function(y) {
                             paste(
                               glue::glue("**For {y}**"),
                               expand_diff(out, path, y),
                               check_rfeed_rating(out, y),
                               check_afeed_rating(out, y),
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

check_rfeed_rating = function(out, x) {
  test = out[out$reviewer == x, 'rfeed']
  if (!is.na(test)) {
    glue::glue("- [ ] Read [review]({test}).")
  }
}

check_rfeed_review = function(out, x) {
  test = out[out$author_random == x, 'rfeed']
  if (!is.na(test)) {
    glue::glue("- [ ] Fill out [review form]({test}).")
  }
}

check_afeed_rating = function(out, x) {
  test = out[out$reviewer == x, 'afeed']
  if (!is.na(test)) {
    glue::glue("- [ ] Fill out [rating form]({test}).")
  }
}


format_folder = function(folder, path) {
  if (!is.null(folder)) {
    glue::glue("{folder}/{path}")
  } else {
    path
  }
}



get_lastcommit_sha = function(repo, path = NULL) {

  arg_is_chr_scalar(repo)
  arg_is_chr(path, allow_null = TRUE)

  if (!is.null(path)) {
    purrr::map_dfr(path,
                   function(path) {
                     sub = get_commits(repo = repo, path = path)
                     sub = sub[order(sub$date, decreasing = TRUE),]
                     tibble::tibble(
                       sha = as.character(sub[1, 'sha']),
                       path = path
                     )
                   })
  } else {
    purrr::map_chr(repo,
                   function(.x) {
                     sub = get_commits(repo = repo)
                     sub = sub[order(sub$date, decreasing = TRUE),]
                     as.character(sub[1, 'sha'])
                   })
  }
}

create_diff_url = function(repo, path) {
  out = purrr::map2_dfc(repo, path,
                  function(x, y) {
                    sha = get_lastcommit_sha(x, y)
                    paste0("https://github.com/", x, "/commit/", sha$sha)
                  }
                  )
  setNames(out, paste0("diff", seq_len(length(out))))
}



peer_place_file = function(repo_files, target_repo, input, message, branch, verbose, overwrite) {

  purrr::walk(input,
              function(y) {

                gh_path = glue::glue("{y[[1]]}/{fs::path_file(y[[2]])}")

                if (!(gh_path %in% repo_files[['path']]) | overwrite) {

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
