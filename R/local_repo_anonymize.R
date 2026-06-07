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
#' * **Git history** - commit author/committer details (and commit messages) are
#'   rewritten so they no longer identify students.
#'
#' @details
#' Anonymizing git history necessarily rewrites every commit, which changes all
#' commit hashes and decouples the repository from its `origin` remote. Pulling
#' from the original remote afterwards would also re-introduce the un-anonymized
#' commits. For this reason the function works on a *copy* by default (the
#' original is left untouched and remains pushable/pullable) and removes the
#' `origin` remote from the copy. The copy is intended as a shareable / archival
#' artifact, not a working clone.
#'
#' The `git_history` argument controls how git is handled:
#'
#' * `"metadata"` (default) - rewrite author/committer name and email on every
#'   commit and scrub commit messages, keeping the full commit history. Uses
#'   `git filter-repo` when available (recommended), otherwise falls back to the
#'   slower `git filter-branch`. Historical file *blobs* are not scrubbed in this
#'   tier (only the working tree and commit metadata).
#' * `"flatten"` - delete `.git` and re-initialise the repo with a single
#'   anonymized commit. Removes all historical PII but loses commit history.
#' * `"none"` - skip git entirely (text contents only).
#'
#' The `"metadata"` tier requires a system `git` on the `PATH`; installing
#' `git-filter-repo` (<https://github.com/newren/git-filter-repo>) is recommended
#' for a faster and more robust rewrite.
#'
#' Only files whose names match `types` are scanned, so binary and other
#' non-text files are skipped by design. Matching is best effort: word boundaries
#' are used for names / NetIDs to avoid clobbering unrelated text, but short or
#' common names may still over-match, and HTML-entity-encoded names are not
#' detected.
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
#' @param git_history Character. How to handle git history, see Details. One of
#'   `"metadata"`, `"flatten"`, or `"none"`.
#' @param remove_origin Logical. Remove the `origin` remote from rewritten repos
#'   (applies to the `"metadata"` and `"flatten"` tiers). Defaults to `TRUE`.
#' @param prompt Logical. Prompt for confirmation before modifying a folder in
#'   place. Defaults to [interactive()].
#'
#' @return Invisibly, a list with two tibbles: `text` (files scrubbed and
#'   replacements made per directory) and `git` (history tier and origin status
#'   per repo).
#'
#' @examples
#' \dontrun{
#' local_repo_anonymize(
#'   "grading/hw1",
#'   roster = "rosters/hw1_roster.csv"
#' )
#'
#' # text only, choosing the columns explicitly
#' local_repo_anonymize(
#'   "grading/hw1",
#'   roster = roster_df,
#'   cols = c(name, netid, email),
#'   git_history = "none"
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
  git_history = c("metadata", "flatten", "none"),
  remove_origin = TRUE,
  prompt = interactive()
) {
  require_gert()
  arg_is_chr_scalar(path)
  arg_is_chr_scalar(output, allow_null = TRUE)
  arg_is_chr(types)
  arg_is_lgl_scalar(remove_origin, prompt)
  git_history = match.arg(git_history)
  cols = rlang::enquo(cols)

  if (!fs::dir_exists(path))
    cli_stop("Unable to locate {.file {path}}.")
  path = as.character(fs::path_real(path))

  git_tool = anon_git_tool()
  if (git_history == "metadata" && is.na(git_tool)) {
    cli_stop(
      'History rewriting ({.val metadata}) requires {.code git} on the system PATH. ',
      'Install git, or use {.code git_history = "flatten"}.'
    )
  }
  if (git_history == "metadata" && git_tool == "filter-branch") {
    cli_warn(
      '{.code git filter-repo} not found; falling back to the slower {.code git filter-branch}. ',
      'Install {.pkg git-filter-repo} for a faster, more robust history rewrite.'
    )
  }

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
    if (prompt && !cli_yeah("Modify {.file {path}} in place? This rewrites files and git history and cannot be undone."))
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
  if (git_history != "none" && length(layout$repos) > 0) {
    git_summ = dplyr::bind_rows(purrr::map(
      layout$repos,
      function(repo) {
        res = purrr::safely(anon_git_dispatch)(repo, map, git_history, git_tool)
        status_msg(
          res,
          "Rewrote git history for {.val {fs::path_file(repo)}}.",
          "Failed to rewrite git history for {.val {fs::path_file(repo)}}."
        )

        origin_removed = FALSE
        if (succeeded(res) && remove_origin && git_history %in% c("metadata", "flatten")) {
          if ("origin" %in% gert::git_remote_list(repo = repo)[["name"]])
            purrr::safely(gert::git_remote_remove)("origin", repo = repo)
          origin_removed = !"origin" %in% gert::git_remote_list(repo = repo)[["name"]]
        }

        tibble::tibble(
          repo = fs::path_file(repo),
          git_history = git_history,
          origin_removed = origin_removed,
          success = succeeded(res)
        )
      }
    ))

    if (remove_origin && git_history %in% c("metadata", "flatten")) {
      cli_warn(
        "Rewriting history decoupled the anonymized repos from GitHub; any ",
        "{.val origin} remote was removed, so they can no longer be pushed or pulled."
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

anon_git_tool = function() {
  git = Sys.which("git")
  if (!nzchar(git))
    return(NA_character_)

  fr = suppressWarnings(
    system2(git, c("filter-repo", "--version"), stdout = TRUE, stderr = TRUE)
  )
  status = attr(fr, "status")
  if (is.null(status) || status == 0L)
    return("filter-repo")

  "filter-branch"
}

anon_git_dispatch = function(repo, map, history, git_tool) {
  switch(
    history,
    metadata = anon_git_metadata(repo, map, git_tool),
    flatten = anon_git_flatten(repo),
    none = NULL
  )
  invisible(TRUE)
}

anon_git_identities = function(repo, git = Sys.which("git")) {
  sep = intToUtf8(31)
  empty = tibble::tibble(name = character(), email = character())

  if (nzchar(git)) {
    fmt = paste0("--format=%an", sep, "%ae", sep, "%cn", sep, "%ce")
    out = suppressWarnings(
      system2(git, c("-C", shQuote(repo), "log", "--all", fmt),
              stdout = TRUE, stderr = FALSE)
    )
    if (length(out) > 0 && is.null(attr(out, "status"))) {
      parts = strsplit(out, sep, fixed = TRUE)
      field = function(i) vapply(parts, function(x) if (length(x) >= i) x[i] else "", character(1))
      ids = tibble::tibble(
        name = c(field(1), field(3)),
        email = c(field(2), field(4))
      )
      return(dplyr::distinct(ids))
    }
  }

  log = purrr::safely(gert::git_log)(repo = repo, max = 1e6)
  if (failed(log))
    return(empty)
  who = c(result(log)[["author"]], result(log)[["committer"]])
  who = who[!is.na(who)]
  if (length(who) == 0)
    return(empty)
  name = trimws(sub("\\s*<.*$", "", who))
  email = sub("^.*<([^>]*)>.*$", "\\1", who)
  email[!grepl("<", who)] = ""
  dplyr::distinct(tibble::tibble(name = name, email = email))
}

anon_resolve_token = function(name, email, map) {
  for (i in seq_len(nrow(map))) {
    lit = map$literal[i]
    if ((nzchar(email) && grepl(lit, email, fixed = TRUE)) ||
        (nzchar(name) && grepl(lit, name, fixed = TRUE)))
      return(map$token[i])
  }
  "anon"
}

anon_resolve_ids = function(repo, map, git) {
  ids = anon_git_identities(repo, git)
  if (nrow(ids) == 0)
    return(ids)
  ids$token = purrr::map2_chr(ids$name, ids$email, anon_resolve_token, map = map)
  ids$new_name = ids$token
  ids$new_email = paste0(ids$token, "@anon.invalid")
  ids
}

anon_git_commit_pending = function(repo) {
  st = purrr::safely(gert::git_status)(repo = repo)
  if (failed(st))
    return(invisible(NULL))
  st = result(st)

  to_add = st$file[st$status %in% c("modified", "typechange", "renamed")]
  if (length(to_add) == 0)
    return(invisible(NULL))

  gert::git_add(to_add, repo = repo)
  purrr::safely(gert::git_commit)(
    "Anonymize repository contents",
    repo = repo,
    author = gert::git_signature("anon", "anon@anon.invalid")
  )
  invisible(NULL)
}

anon_git_metadata = function(repo, map, git_tool) {
  git = Sys.which("git")
  anon_git_commit_pending(repo)

  ids = anon_resolve_ids(repo, map, git)
  if (nrow(ids) == 0)
    return(invisible(NULL))

  if (identical(git_tool, "filter-repo"))
    anon_filter_repo(repo, ids, map, git)
  else
    anon_filter_branch(repo, ids, map, git)
}

anon_filter_branch = function(repo, ids, map, git) {
  em = ids[nzchar(ids$email), c("email", "new_name", "new_email")]
  em = em[!duplicated(em$email), ]

  dq = function(x) paste0('"', x, '"')
  branches = sprintf(
    "    %s) N=%s; E=%s;;",
    dq(anon_glob_escape(em$email)), dq(em$new_name), dq(em$new_email)
  )
  branches = paste(branches, collapse = "\n")

  block = function(in_email, out_name, out_email) {
    paste0(
      'case "$', in_email, '" in\n',
      branches, "\n",
      '    *) N="anon"; E="anon@anon.invalid";;\n',
      "esac\n",
      "export ", out_name, '="$N"\n',
      "export ", out_email, '="$E"\n'
    )
  }
  env_filter = paste0(
    block("GIT_AUTHOR_EMAIL", "GIT_AUTHOR_NAME", "GIT_AUTHOR_EMAIL"),
    block("GIT_COMMITTER_EMAIL", "GIT_COMMITTER_NAME", "GIT_COMMITTER_EMAIL")
  )

  args = c(
    "-C", shQuote(repo), "filter-branch", "-f",
    "--env-filter", shQuote(env_filter)
  )

  msg_script = anon_perl_msg_script(map)
  perl = Sys.which("perl")
  if (!is.null(msg_script) && nzchar(perl))
    args = c(args, "--msg-filter", shQuote(paste("perl", shQuote(msg_script))))

  args = c(args, "--", "--all")

  out = suppressWarnings(
    system2(git, args, env = "FILTER_BRANCH_SQUELCH_WARNING=1",
            stdout = TRUE, stderr = TRUE)
  )
  status = attr(out, "status")
  if (!is.null(status) && status != 0L)
    stop(paste(out, collapse = "\n"), call. = FALSE)

  orig = fs::path(repo, ".git", "refs", "original")
  if (fs::dir_exists(orig))
    fs::dir_delete(orig)
  suppressWarnings(system2(git, c("-C", shQuote(repo), "reflog", "expire", "--expire=now", "--all"), stdout = TRUE, stderr = TRUE))
  suppressWarnings(system2(git, c("-C", shQuote(repo), "gc", "--prune=now", "--quiet"), stdout = TRUE, stderr = TRUE))
  invisible(NULL)
}

anon_filter_repo = function(repo, ids, map, git) {
  ids = ids[nzchar(ids$email), ]
  old = ifelse(
    nzchar(ids$name),
    paste0(ids$name, " <", ids$email, ">"),
    paste0("<", ids$email, ">")
  )
  mailmap = paste0(ids$new_name, " <", ids$new_email, "> ", old)
  mm_file = tempfile(fileext = ".mailmap")
  writeLines(mailmap, mm_file)

  rules_file = tempfile(fileext = ".txt")
  writeLines(paste0(map$literal, "==>", map$token), rules_file)

  out = suppressWarnings(system2(
    git,
    c("-C", shQuote(repo), "filter-repo", "--force",
      "--mailmap", shQuote(mm_file),
      "--replace-message", shQuote(rules_file)),
    stdout = TRUE, stderr = TRUE
  ))
  status = attr(out, "status")
  if (!is.null(status) && status != 0L)
    stop(paste(out, collapse = "\n"), call. = FALSE)
  invisible(NULL)
}

anon_git_flatten = function(repo) {
  fs::dir_delete(fs::path(repo, ".git"))
  gert::git_init(repo)
  gert::git_add(".", repo = repo)
  gert::git_commit(
    "Anonymized snapshot",
    repo = repo,
    author = gert::git_signature("anon", "anon@anon.invalid")
  )
  invisible(NULL)
}

anon_glob_escape = function(x) {
  gsub("([\\\\*?\\[])", "\\\\\\1", x, perl = TRUE)
}

anon_perl_msg_script = function(map) {
  ok = !grepl("[{}\\\\]", map$literal)
  m = map[ok, ]
  if (nrow(m) == 0)
    return(NULL)
  script = c(
    "while (<>) {",
    sprintf("  s{\\Q%s\\E}{%s}g;", m$literal, m$token),
    "  print;",
    "}"
  )
  f = tempfile(fileext = ".pl")
  writeLines(script, f)
  f
}
