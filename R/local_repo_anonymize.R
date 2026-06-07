#' @title Anonymize a local repo or grading project
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Makes a best-effort pass over a local git repository, a directory of
#' repositories, or a grading project created by [org_grade_assignment()] and
#' removes student personally identifying information (PII). Two things happen:
#'
#' * **File contents** - every text file is scanned and any roster-derived value
#'   (names, NetIDs, emails, GitHub usernames, ...) is replaced with a stable
#'   per-student token (e.g. `student_07`).
#' * **Git history** - each repository's `.git` directory is either deleted
#'   outright (the default) or kept untouched.
#'
#' @details
#' Git history cannot be reliably anonymized while also being preserved. Even
#' after rewriting commit author and committer identities, the *original file
#' contents* of older commits remain recoverable (`git show <old-sha>:README.md`),
#' and real names routinely survive in commit messages and branch names. Because
#' anyone you hand the repository to receives its full history, there are only two
#' honest options, selected with `git_history`:
#'
#' * `"delete"` (default) - remove each repository's `.git` directory entirely.
#'   The result is the scrubbed working tree with no recoverable history. This is
#'   the safe choice for sharing student work.
#' * `"keep"` - leave `.git` untouched. The working-tree files are still scrubbed,
#'   but the original commit history (with real names and emails) is retained, so
#'   the result is **not** safe to distribute.
#'
#' The function works on a *copy* by default (`<path>_anon`), leaving the original
#' intact, so deleting history is non-destructive to your working copy.
#'
#' Only files whose names match `types` are scanned, so binary and other non-text
#' files are skipped by design. Matching is best effort: word boundaries are used
#' for names / NetIDs to avoid clobbering unrelated text, but short or common
#' names may still over-match, and HTML-entity-encoded names are not detected.
#' Anonymization is roster-driven, so anyone absent from the roster (e.g. a
#' student who later dropped) will not be matched.
#'
#' @param path Character. Path to a git repository, a directory of repositories,
#'   or a grading project folder (one containing a `repos/` subdirectory of
#'   cloned repositories, e.g. as produced by [org_grade_assignment()]).
#' @param roster Either a path to a roster CSV file or a data frame. Each row is
#'   treated as one student.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Roster columns whose
#'   values should be stripped from the contents. Defaults to the `name`,
#'   `first`, `last`, `email`, `netid`, and `github` columns when present.
#' @param output Character. Directory to write the anonymized copy to. Defaults
#'   to `<path>_anon`. Set `output = path` to modify the folder in place (gated by
#'   a confirmation prompt when `prompt = TRUE`).
#' @param types Character. Regular expressions matched against the end of each
#'   file name to decide which files are treated as text and scanned. Defaults to
#'   common text formats found in student work (markdown, R / Python source,
#'   notebooks, html, csv, yaml, ...).
#' @param git_history Character. How each repository's git history is handled.
#'   Either `"delete"` (default, remove the `.git` directory entirely) or `"keep"`
#'   (leave it untouched, retaining the un-anonymized history).
#' @param prompt Logical. Prompt for confirmation before modifying a folder in
#'   place. Defaults to [interactive()].
#'
#' @return Invisibly, a list with two tibbles: `text` (files scrubbed and
#'   replacements made per directory) and `git` (the action taken per repo).
#'
#' @examples
#' \dontrun{
#' local_repo_anonymize(
#'   "grading/hw1",
#'   roster = "rosters/hw1_roster.csv"
#' )
#'
#' # scrub text but keep history, choosing the columns explicitly
#' local_repo_anonymize(
#'   "grading/hw1",
#'   roster = roster_df,
#'   cols = c(name, netid, email),
#'   git_history = "keep"
#' )
#' }
#'
#' @export
#'
local_repo_anonymize = function(
  path,
  roster,
  cols = tidyselect::any_of(c("name", "first", "last", "email", "netid", "github")),
  output = NULL,
  types = c(
    ".md", ".qmd", ".[Rr]md", ".txt", ".csv", ".tsv", ".html?", ".[Rr]",
    ".py", ".ipynb", ".json", ".ya?ml", ".tex", ".bib", ".toml", ".[Rr]proj"
  ),
  git_history = c("delete", "keep"),
  prompt = interactive()
) {
  require_gert()
  arg_is_chr_scalar(path)
  arg_is_chr_scalar(output, allow_null = TRUE)
  arg_is_chr(types)
  arg_is_lgl_scalar(prompt)
  git_history = match.arg(git_history)
  cols = rlang::enquo(cols)

  if (!fs::dir_exists(path))
    cli_stop("Unable to locate {.file {path}}.")
  path = as.character(fs::path_real(path))

  # The roster + map are validated before we copy anything potentially large.
  roster = anon_read_roster(roster)
  map = anon_build_map(roster, cols)
  if (nrow(map) == 0)
    cli_stop("No PII values found in the selected roster columns.")

  if (is.null(output))
    output = paste0(path, "_anon")
  output = fs::path_expand(output)
  in_place = identical(as.character(fs::path_norm(output)), path)

  if (in_place) {
    if (prompt && !cli_yeah("Modify {.file {path}} in place? This rewrites files and deletes git history and cannot be undone."))
      return(invisible(NULL))
    work = path
  } else {
    if (fs::dir_exists(output))
      cli_stop("Output directory {.file {output}} already exists; remove it or choose another {.arg output}.")
    cli::cli_alert_info("Copying {.file {path}} to {.file {output}}.")
    fs::dir_copy(path, output)
    work = as.character(fs::path_real(output))
  }

  layout = anon_detect_layout(work)

  cli::cli_alert_info("Scrubbing text in {.val {length(layout$text_dirs)}} director{?y/ies}.")
  text_summ = purrr::map(
    layout$text_dirs,
    function(d) {
      r = anon_scrub_dir(d, map, types)
      cli::cli_alert_success(
        "Scrubbed {.val {r$replacements}} value{?s} in {.val {r$files}} file{?s} under {.file {fs::path_file(d)}}."
      )
      r
    }
  )

  git_summ = tibble::tibble()
  if (length(layout$repos) > 0) {
    if (git_history == "delete") {
      git_summ = dplyr::bind_rows(purrr::map(
        layout$repos,
        function(repo) {
          res = purrr::safely(anon_git_delete)(repo)
          status_msg(
            res,
            "Deleted git history for {.val {fs::path_file(repo)}}.",
            "Failed to delete git history for {.val {fs::path_file(repo)}}."
          )
          tibble::tibble(
            repo = fs::path_file(repo),
            git_history = git_history,
            deleted = succeeded(res)
          )
        }
      ))
    } else {
      git_summ = tibble::tibble(
        repo = fs::path_file(layout$repos),
        git_history = git_history,
        deleted = FALSE
      )
      cli_warn(
        "Git history was kept; the original commit history (including real names ",
        "and emails) is retained, so the result is not safe to distribute."
      )
    }
  }

  cli::cli_alert_success("Anonymized copy written to {.file {work}}.")

  invisible(list(
    text = tibble::tibble(
      dir = as.character(fs::path_rel(layout$text_dirs, work)),
      files = purrr::map_int(text_summ, "files"),
      replacements = purrr::map_int(text_summ, "replacements")
    ),
    git = git_summ
  ))
}


# Layout detection -------------------------------------------------------------

anon_detect_layout = function(path) {
  path = as.character(fs::path_real(path))

  if (is_git_repo(path))
    return(list(kind = "repo", repos = path, text_dirs = path))

  # A grading project (e.g. from org_grade_assignment()) keeps the student
  # repos under a `repos/` subdirectory alongside artifact folders.
  repos_dir = fs::path(path, "repos")
  repos = character()
  if (fs::dir_exists(repos_dir)) {
    repos = as.character(fs::dir_ls(repos_dir, type = "directory"))
    repos = repos[purrr::map_lgl(repos, is_git_repo)]
  }

  if (length(repos) > 0) {
    top = as.character(fs::dir_ls(path, type = "directory"))
    key = top[purrr::map_lgl(top, is_git_repo)]
    return(list(
      kind = "grading",
      repos = unique(c(repos, key)),
      text_dirs = top
    ))
  }

  dirs = as.character(repo_dir_helper(path))
  list(
    kind = "repo_dir",
    repos = dirs[purrr::map_lgl(dirs, is_git_repo)],
    text_dirs = path
  )
}


# Roster + replacement map -----------------------------------------------------

anon_read_roster = function(roster) {
  if (is.character(roster)) {
    arg_is_chr_scalar(roster)
    if (!fs::file_exists(roster))
      cli_stop("Unable to locate roster file {.file {roster}}.")
    roster = readr::read_csv(roster, show_col_types = FALSE)
  }
  arg_is_df(roster)
  roster
}

anon_build_map = function(roster, cols) {
  loc = tidyselect::eval_select(cols, data = roster)
  if (length(loc) == 0)
    cli_stop("No matching columns were selected from the roster via {.arg cols}.")

  pii = roster[loc]
  n = nrow(pii)
  width = max(2L, nchar(as.character(n)))
  tokens = sprintf(paste0("student_%0", width, "d"), seq_len(n))

  map = tibble::tibble(
    literal = trimws(unlist(lapply(pii, as.character), use.names = FALSE)),
    token = rep(tokens, times = ncol(pii))
  )

  map = map[!is.na(map$literal) & nzchar(map$literal), ]

  short = map$literal[nchar(map$literal) < 2L]
  if (length(short) > 0) {
    cli_warn(
      "Ignoring PII value{?s} shorter than 2 characters: {.val {unique(short)}}."
    )
    map = map[nchar(map$literal) >= 2L, ]
  }

  map = dplyr::distinct(map, .data$literal, .data$token)

  dup = unique(map$literal[duplicated(map$literal)])
  if (length(dup) > 0) {
    cli_warn(
      "Value{?s} {.val {dup}} appear for multiple students and will be replaced ",
      "with a single token; those occurrences cannot be distinguished."
    )
    map = map[!duplicated(map$literal), ]
  }

  map$is_email = grepl("@", map$literal, fixed = TRUE)
  map$regex = anon_build_regex(map$literal, map$is_email)
  map = map[order(-nchar(map$literal), -map$is_email), ]
  map
}

anon_build_regex = function(literal, is_email) {
  quoted = paste0("\\Q", literal, "\\E")
  left = ifelse(grepl("^\\w", literal, perl = TRUE), "\\b", "")
  right = ifelse(grepl("\\w$", literal, perl = TRUE), "\\b", "")
  ifelse(is_email, quoted, paste0(left, quoted, right))
}


# Text scrubbing ---------------------------------------------------------------

anon_match_types = function(files, types) {
  pattern = paste0("(", paste(types, collapse = "|"), ")$")
  grepl(pattern, fs::path_file(files))
}

anon_is_binary = function(file) {
  con = file(file, "rb")
  on.exit(close(con))
  raw = readBin(con, what = "raw", n = 8000L)
  length(raw) > 0L && any(raw == as.raw(0L))
}

anon_scrub_file = function(file, map) {
  txt = readr::read_file(file)
  if (length(txt) == 0L || is.na(txt) || !nzchar(txt))
    return(0L)

  present = vapply(map$literal, function(l) grepl(l, txt, fixed = TRUE), logical(1))
  if (!any(present))
    return(0L)

  n_repl = 0L
  for (i in which(present)) {
    m = gregexpr(map$regex[i], txt, perl = TRUE)[[1]]
    cnt = if (length(m) == 1L && m[1] == -1L) 0L else length(m)
    if (cnt > 0L) {
      txt = gsub(map$regex[i], map$token[i], txt, perl = TRUE)
      n_repl = n_repl + cnt
    }
  }

  if (n_repl > 0L)
    readr::write_file(txt, file)

  n_repl
}

anon_scrub_dir = function(dir, map, types) {
  files = as.character(fs::dir_ls(dir, recurse = TRUE, type = "file", all = TRUE))
  files = files[!grepl("(^|/)\\.git/", files)]
  files = files[anon_match_types(files, types)]

  files_changed = 0L
  total = 0L
  for (f in files) {
    if (anon_is_binary(f))
      next
    res = tryCatch(anon_scrub_file(f, map), error = function(e) NA_integer_)
    if (is.na(res)) {
      cli_warn("Failed to scrub {.file {f}}.")
      next
    }
    if (res > 0L) {
      files_changed = files_changed + 1L
      total = total + res
    }
  }

  list(files = files_changed, replacements = total)
}


# Git history ------------------------------------------------------------------

anon_git_delete = function(repo) {
  git_dir = fs::path(repo, ".git")
  if (fs::dir_exists(git_dir))
    fs::dir_delete(git_dir)
  else if (fs::file_exists(git_dir))
    fs::file_delete(git_dir)
  invisible(TRUE)
}
