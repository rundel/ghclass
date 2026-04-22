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
#'   regex patterns passed as `filter` to [action_artifact_download()] (matched
#'   against artifact names). E.g. `c("html" = "html-output", "report" = "pdf-report")`.
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
  repo_names = get_repo_name(repos)

  if (length(artifacts) > 0) {
    cli::cli_alert_info("Gathering artifacts.")

    for (nm in names(artifacts)) {
      art_dir = file.path(path, nm)
      res[["artifacts"]][[nm]] = action_artifact_download(
        repos, art_dir, filter = artifacts[[nm]]
      )
    }

    for (nm in names(artifacts)) {
      art_dir = file.path(path, nm)
      if (!dir.exists(art_dir)) next

      subdirs = list.dirs(art_dir, recursive = FALSE, full.names = FALSE)

      missing = purrr::keep(repo_names, function(rn) {
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
