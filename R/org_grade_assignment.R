#' @title Setup grading for an assignment
#'
#' @description
#' This is a higher level function that automates the common steps for
#' setting up grading of an assignment:
#'
#' * Find repos matching a filter pattern in an organization
#' * Clone all matched repos locally
#' * Download GitHub Actions artifacts (e.g. rendered html, md files)
#' * Report any missing artifacts
#' * Create comment template files for each repo
#'
#' @param path Character. Root directory for the grading folder (created if it doesn't exist).
#' @param org Character. Name of the GitHub organization.
#' @param repo_filter Character. Regex pattern passed to [org_repos()]'s `filter`
#'   argument to select repos (e.g. `"hw01_"`).
#' @param artifacts Named character vector. Names are subfolder names, values are
#'   file patterns for [action_artifact_download()]. E.g.
#'   `c("html" = ".*\\.html", "md" = ".*\\.md")`.
#' @param comment_template Character. Template text used to populate each comment
#'   markdown file. Defaults to `""`.
#' @param key_repo Character. Optional repository address in `owner/name` format
#'   to clone into the root of the grading folder as an answer key.
#'
#' @return An invisible list containing:
#'   * `repos` — character vector of matched repo addresses
#'   * `cloned` — result from [local_repo_clone()]
#'   * `artifacts` — named list of results from each [action_artifact_download()] call
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
  key_repo = NULL
) {
  arg_is_chr_scalar(path, org, repo_filter, comment_template)
  arg_is_chr_scalar(key_repo, allow_null = TRUE)
  arg_is_chr(artifacts)

  if (length(artifacts) > 0 && is.null(names(artifacts))) {
    cli_stop("{.arg artifacts} must be a named character vector.")
  }

  repos = org_repos(org, repo_filter)

  if (length(repos) == 0) {
    cli_stop("No repos found in {.val {org}} matching {.val {repo_filter}}.")
  }

  res = list(repos = repos)

  # Clone repos
  repos_dir = file.path(path, "repos")
  cli::cli_alert_info("Cloning {.val {length(repos)}} student repo{?s} matching {.val {repo_filter}}.")
  if (dir.exists(repos_dir)) {
    cli::cli_alert_warning("Directory {.file {repos_dir}} already exists, skipping clone.")
  } else {
    res[["cloned"]] = local_repo_clone(repos, repos_dir)
  }

  # Download artifacts
  res[["artifacts"]] = list()
  repo_names = get_repo_name(repos)

  if (length(artifacts) > 0) {
    cli::cli_alert_info("Gathering artifacts.")

    for (nm in names(artifacts)) {
      art_dir = file.path(path, nm)
      if (dir.exists(art_dir)) {
        cli::cli_alert_warning("Directory {.file {art_dir}} already exists, skipping {.val {nm}} artifact download.")
      } else {
        res[["artifacts"]][[nm]] = action_artifact_download(
          repos, art_dir, file_pat = artifacts[[nm]]
        )
      }
    }

    # Report missing artifacts
    for (nm in names(artifacts)) {
      art_dir = file.path(path, nm)
      if (!dir.exists(art_dir)) next

      downloaded = tools::file_path_sans_ext(dir(art_dir))
      missing = setdiff(repo_names, downloaded)

      if (length(missing) > 0) {
        cli::cli_alert_warning(
          "Missing {.val {nm}} artifacts for {.val {length(missing)}} repo{?s}: {.val {missing}}."
        )
      }
    }
  }

  # Create comment files
  comments_dir = file.path(path, "comments")
  cli::cli_alert_info("Creating comment files.")
  if (dir.exists(comments_dir)) {
    cli::cli_alert_warning("Directory {.file {comments_dir}} already exists, skipping comment file creation.")
  } else {
    dir.create(comments_dir, showWarnings = FALSE, recursive = TRUE)

    comment_files = file.path(comments_dir, paste0(repo_names, ".md"))

    purrr::walk(
      comment_files,
      writeLines,
      text = comment_template
    )

    cli::cli_alert_success("Created {.val {length(comment_files)}} comment file{?s} in {.file {comments_dir}}.")
    res[["comments"]] = comment_files
  }

  # Clone key repo
  if (!is.null(key_repo)) {
    cli::cli_alert_info("Cloning key repo {.val {key_repo}}.")
    key_dir = file.path(path, get_repo_name(key_repo))
    if (dir.exists(key_dir)) {
      cli::cli_alert_warning("Directory {.file {key_dir}} already exists, skipping key repo clone.")
    } else {
      res[["key"]] = local_repo_clone(key_repo, path)
    }
  }

  invisible(res)
}
