github_api_peer_issue_create = function(repo, title, body, assignee, labels) {
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

  purrr::pmap_dfr(
    list(name, color, desc),
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


peer_issue_label_apply = function(org, repo = NULL, filter = NULL,
                                  exclude = FALSE, verbose = FALSE,
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

  cli::cli_alert_info("Applying labels: This might take a moment ...")
  out = purrr::map_dfr(repo, ~ peer_issue_label_create(.x))

  # TODO: Improve messaging about label creation
  # Note that the 422 error likely means that the label already exists.
  if (verbose)
    peer_issue_label_apply_msg(out)

  if (show_label_result)
    out

}

peer_issue_label_apply_msg = function(label_df) {
  purrr::walk(
    unique(label_df[["label"]]),
    function(x) {
      repo_label_yes = label_df[["repo"]][label_df[["label"]] == x & label_df[["created"]]]
      repo_label_no = label_df[["repo"]][label_df[["label"]] == x & !label_df[["created"]]]

      n_yes = length(repo_label_yes)
      n_no = length(repo_label_no)

      if (n_yes > 0)
        cli::cli_alert_success(
          "Created label {.val {x}} for {.val {n_yes}} repositories."
        )

      if (n_no > 0)
        cli::cli_alert_danger(
          "Failed to create label {.val {x}} for {.val {n_no}} repositories. The label may already exist. Re-run with `show_label_result = TRUE` for more information."
        )
    }
  )
}

peer_issue_create = function(out, title, step = c("review", "rating"),
                             org, prefix, suffix, branch = "master") {
  arg_is_chr_scalar(step, prefix, suffix, org, branch, title)

  if (is.null(out[['repo']])) {
    cli::cli_alert_danger("Skipping issue creation: no files found for any repositories.")
  }

  purrr::walk(
    unique(out[['repo']]),
    function(r) {
      sub = out[out[['repo']] == r, ]

      url_start = list(
        blob = paste0("https://github.com/", r, "/blob/", branch, "/"),
        tree = paste0("https://github.com/", r, "/tree/", branch, "/"),
        commit = paste0("https://github.com/", r, "/commit/")
      )

      if (step == "review") {
        body = peer_issue_body_review(sub = sub, url_start = url_start)
        labels = c(":pencil: Complete review")
      } else if (step == "rating") {
        body = peer_issue_body_rating(sub = sub, url_start = url_start)
        labels = c(":mag: Inspect review")
      }

      assignee = peer_repo_get_user(
        repo = r,
        org = org,
        prefix = prefix,
        suffix = suffix
      )

      issue_create(
        repo = r,
        title = title,
        body = body,
        labels = labels,
        assignees = assignee
      )
    }
  )
}


peer_issue_body_review = function(sub, url_start) {
  aut = unique(sub[['target_folder']])

  rev_txt = purrr::map_chr(
    aut,
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
    }
  )

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
              sub[['target_folder']] == aut,]

  if (nrow(tmp) > 0) {
    url = glue::glue("{url_start[['tree']]}{aut}")
    glue::glue("- [ ] Review [assignment file(s)]({url}).")
  }
}

issue_txt_complete_review = function(sub, aut, url_start) {
  tmp = sub[sub[['category']] == "review" &
              sub[['target_folder']] == aut,]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start[['blob']]}{tmp[['path']]}")
    path_txt = sub(glue::glue("{aut}/"), "", tmp[['path']])
    glue::glue("- [ ] Fill out review form: [{path_txt}]({url}).")
  }
}


peer_issue_body_rating = function(sub, url_start = url_start) {
  rev = unique(sub[['target_folder']])

  rev_txt = purrr::map_chr(
    rev,
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
    }
  )

  glue::glue(
    "The feedback from your peers has been added to your repository.\n\n",
    "To finish the assignment, please pull changes into your assignment repository and complete the tasks below. Don't forget to commit and push your work when you are done.\n\n",
    paste(rev_txt, collapse = "\n\n"),
    "\n\n\n",
    "*You may use the check boxes to keep track of your progress*."
  )
}

issue_txt_diff = function(sub, rev, url_start = url_start) {
  tmp = sub[sub[['category']] == "assignment" & sub[['target_folder']] == rev,]

  diff_txt = purrr::map_chr(
    seq_len(nrow(tmp)),
    function(z) {
      path_txt = sub(glue::glue("{rev}/"), "", tmp[['path']][z])
      if (!is.na(tmp[['commit_sha']][z])) {
        commit_url = paste0(url_start[['commit']], tmp[['commit_sha']][z])
        glue::glue("- [ ] Review changes suggested for [{path_txt}]({commit_url}).")
      } else {
        blob_url = paste0(url_start[['blob']], tmp[['path']][z])
        glue::glue("- [ ] No direct edits were made to [{path_txt}]({blob_url}).")
      }
    }
  )
  paste(diff_txt, collapse = "\n")
}

issue_txt_read_review = function(sub, rev, url_start = url_start) {
  tmp = sub[sub[['category']] == "review" & sub[['target_folder']] == rev,]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start[['blob']]}{tmp[['path']]}")
    path_txt = sub(glue::glue("{rev}/"), "", tmp[['path']])
    glue::glue("- [ ] Read review: [{path_txt}]({url}).")
  }
}


issue_txt_complete_rating = function(sub, rev, url_start = url_start) {
  tmp = sub[sub[['category']] == "rating" &
              sub[['target_folder']] == rev,]

  if (nrow(tmp) > 0) {
    arg_is_chr_scalar(tmp[['path']])
    url = glue::glue("{url_start[['blob']]}{tmp[['path']]}")
    path_txt = sub(glue::glue("{rev}/"), "", tmp[['path']])
    glue::glue("- [ ] Fill out rating form: [{path_txt}]({url}).")
  }
}
