#' @title Setup grading for an assignment
#'
#' @description
#' This is a higher level function that automates the common steps for
#' setting up grading of an assignment:
#'
#' * Find repos matching a filter pattern in an organization
#' * Clone all matched repos locally
#' * Download GitHub Actions artifacts (e.g. rendered html, md files)
#' * Report any missing or out of sync artifacts
#' * Create comment template files for each repo
#'
#' Artifacts are considered out of sync when they were built from a commit
#' other than the current commit of the cloned repo (e.g. the student pushed
#' after the last successful workflow run). These are skipped by default,
#' see `allow_stale`.
#'
#' @param path Character. Root directory for the grading folder (created if it doesn't exist).
#' @param org Character. Name of the GitHub organization.
#' @param repo_filter Character. Regex pattern passed to [org_repos()]'s `filter`
#'   argument to select repos (e.g. `"hw01_"`).
#' @param artifacts Named character vector. Names are subfolder names, values are
#'   regex patterns passed as `filter` to [action_artifacts()] (matched
#'   against artifact names). E.g. `c("html" = "html-output", "report" = "pdf-report")`.
#' @param comment_template Character. Template text used to populate each comment
#'   markdown file. Defaults to `""`.
#' @param key_repo Character. Optional repository address in `owner/name` format
#'   to clone into the root of the grading folder as an answer key.
#' @param allow_stale Logical. Should out of sync artifacts be downloaded.
#'   Default `FALSE`, in which case they are reported and skipped.
#'
#' @return An invisible list containing:
#'   * `repos` — character vector of matched repo addresses
#'   * `cloned` — result from [local_repo_clone()]
#'   * `artifacts` — named list of results from each [action_artifact_download()] call
#'   * `stale` — named list of tibbles of out of sync artifacts for each entry of `artifacts`
#'   * `comments` — character vector of comment file paths created
#'   * `key` — result from cloning the key repo (if provided)
#'
#' @export
#'
org_grade_assignment = function(
  path,
  org,
  repo_filter,
  artifacts = character(),
  comment_template = "",
  key_repo = NULL,
  allow_stale = FALSE
) {
  arg_is_chr_scalar(path, org, repo_filter, comment_template)
  arg_is_chr_scalar(key_repo, allow_null = TRUE)
  arg_is_chr(artifacts)
  arg_is_lgl_scalar(allow_stale)

  if (length(artifacts) > 0 && is.null(names(artifacts))) {
    cli_stop("{.arg artifacts} must be a named character vector.")
  }

  if (dir.exists(path)) {
    cli_stop("Destination directory {.file {path}} already exists, please remove it or choose a different path.")
  }

  if (!is.null(key_repo) && !repo_exists(key_repo, quiet = TRUE)) {
    cli_stop("Key repo {.val {key_repo}} does not exist.")
  }

  repos = org_repos(org, repo_filter)

  if (length(repos) == 0) {
    cli_stop("No repos found in {.val {org}} matching {.val {repo_filter}}.")
  }

  res = list(repos = repos)

  repos_dir = file.path(path, "repos")
  cli::cli_alert_info("Cloning {.val {length(repos)}} student repo{?s} matching {.val {repo_filter}}.")
  res[["cloned"]] = local_repo_clone(repos, repos_dir)

  res[["artifacts"]] = list()
  res[["stale"]] = list()
  repo_names = get_repo_name(repos)

  if (length(artifacts) > 0) {
    cli::cli_alert_info("Gathering artifacts.")

    head_shas = local_repo_head_sha(res[["cloned"]])
    skipped = list()

    for (nm in names(artifacts)) {
      ids = flag_stale_artifacts(
        action_artifacts(repos, filter = artifacts[[nm]]),
        head_shas
      )
      stale = ids[ids[["stale"]], ]

      if (nrow(stale) > 0) {
        report_stale_artifacts(nm, stale, allow_stale)
        if (!allow_stale)
          ids = ids[!ids[["stale"]], ]
      }

      res[["stale"]][[nm]] = dplyr::select(stale, -"stale")
      skipped[[nm]] = setdiff(stale[["repo"]], ids[["repo"]])

      download_repos = setdiff(repos, skipped[[nm]])
      res[["artifacts"]][[nm]] = if (length(download_repos) == 0) {
        character()
      } else {
        action_artifact_download(download_repos, file.path(path, nm), ids = ids)
      }
    }

    for (nm in names(artifacts)) {
      art_dir = file.path(path, nm)
      if (!dir.exists(art_dir)) next

      subdirs = list.dirs(art_dir, recursive = FALSE, full.names = FALSE)

      missing = purrr::keep(setdiff(repo_names, get_repo_name(skipped[[nm]])), function(rn) {
        !any(subdirs == rn | startsWith(subdirs, paste0(rn, "_")))
      })

      if (length(missing) > 0) {
        cli::cli_alert_warning(
          "Missing {.val {nm}} artifacts for {.val {length(missing)}} repo{?s}: {.val {missing}}."
        )
      }
    }
  }

  comments_dir = file.path(path, "comments")
  cli::cli_alert_info("Creating comment files.")
  dir.create(comments_dir, showWarnings = FALSE, recursive = TRUE)

  comment_files = file.path(comments_dir, paste0(repo_names, ".md"))

  purrr::walk(
    comment_files,
    writeLines,
    text = comment_template
  )

  cli::cli_alert_success("Created {.val {length(comment_files)}} comment file{?s} in {.file {comments_dir}}.")
  res[["comments"]] = comment_files

  if (!is.null(key_repo)) {
    cli::cli_alert_info("Cloning key repo {.val {key_repo}}.")
    res[["key"]] = local_repo_clone(key_repo, path)
  }

  invisible(res)
}


local_repo_head_sha = function(dirs) {
  purrr::map_chr(
    dirs,
    function(dir) {
      if (is.na(dir))
        return(NA_character_)

      res = purrr::safely(gert::git_commit_id)(repo = dir)
      ternary(succeeded(res), result(res), NA_character_)
    }
  )
}

# An artifact is stale when it was built from a commit other than the repo's
# current head; if either commit is unknown we can't tell, so it is not flagged.
flag_stale_artifacts = function(ids, head_sha) {
  repo_commit = unname(head_sha[ids[["repo"]]])

  ids[["repo_commit"]] = repo_commit
  ids[["stale"]] = !is.na(repo_commit) & !is.na(ids[["commit"]]) & ids[["commit"]] != repo_commit

  ids
}

report_stale_artifacts = function(nm, stale, allow_stale) {
  n = nrow(stale)

  if (allow_stale) {
    cli::cli_alert_warning(paste(
      "{.val {nm}}: {.val {n}} artifact{?s} {?is/are} out of sync with the current commit of the repo,",
      "downloading {?it/them} anyway since {.code allow_stale = TRUE}:"
    ))
  } else {
    cli::cli_alert_warning(paste(
      "{.val {nm}}: skipping {.val {n}} artifact{?s} that {?is/are} out of sync with the current commit of the repo,",
      "set {.code allow_stale = TRUE} to download {?it/them} anyway:"
    ))
  }

  cli::cli_ul()
  purrr::pwalk(
    stale[c("repo", "name", "commit", "repo_commit")],
    function(repo, name, commit, repo_commit) {
      cli::cli_li(paste(
        "{.val {repo}}: artifact {.val {name}} built from {.field {substr(commit, 1, 7)}},",
        "repo is at {.field {substr(repo_commit, 1, 7)}}"
      ))
    }
  )
  cli::cli_end()
}
