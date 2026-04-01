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
#'
#' @return An invisible list containing:
#'   * `repos` — character vector of matched repo addresses
#'   * `cloned` — result from [local_repo_clone()]
#'   * `artifacts` — named list of results from each [action_artifact_download()] call
#'   * `comments` — character vector of comment file paths created
#'
#' @export
#'
org_grade_assignment = function(
  path,
  org,
  repo_filter,
  artifacts = character(),
  comment_template = ""
) {
  arg_is_chr_scalar(path, org, repo_filter, comment_template)
  arg_is_chr(artifacts)

  if (length(artifacts) > 0 && is.null(names(artifacts))) {
    cli_stop("{.arg artifacts} must be a named character vector.")
  }

  repos = org_repos(org, repo_filter)

  if (length(repos) == 0) {
    cli_stop("No repos found in {.val {org}} matching {.val {repo_filter}}.")
  }

  cli::cli_alert_info("Found {.val {length(repos)}} repo{?s} matching {.val {repo_filter}}.")

  res = list(repos = repos)

  # Clone repos
  res[["cloned"]] = local_repo_clone(repos, file.path(path, "repos"))

  # Download artifacts
  res[["artifacts"]] = list()
  repo_names = get_repo_name(repos)

  for (nm in names(artifacts)) {
    art_dir = file.path(path, nm)
    res[["artifacts"]][[nm]] = action_artifact_download(
      repos, art_dir, file_pat = artifacts[[nm]]
    )
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

  # Create comment files
  comments_dir = file.path(path, "comments")
  dir.create(comments_dir, showWarnings = FALSE, recursive = TRUE)

  comment_files = file.path(comments_dir, paste0(repo_names, ".md"))

  purrr::walk(
    comment_files,
    writeLines,
    text = comment_template
  )

  cli::cli_alert_success("Created {.val {length(comment_files)}} comment file{?s} in {.file {comments_dir}}.")

  res[["comments"]] = comment_files

  invisible(res)
}
