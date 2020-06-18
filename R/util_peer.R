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
  i = seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}




# Grab reviewer for author
peer_get_rev = function(aut, roster, out = c("rev", "rev_random", "rev_no")) {
  out = match.arg(out)

  m = seq_len(length(names(roster)[grepl("^rev[0-9]+$", names(roster))]))

  rev_random = as.character(roster[roster[["user"]] == aut, paste0("rev", m)])
  rev = roster[["user"]][purrr::map_int(rev_random, ~ which(roster[["user_random"]] == .x))]
  rev_no = names(roster)[purrr::map_int(rev_random, ~ which(roster[roster[["user"]] == aut, ] == .x))]

  if (out == "rev") {
    rev
  } else if (out == "rev_random") {
    rev_random
  } else if (out == "rev_no") {
    rev_no
  }
}






format_folder = function(folder, path) {
  if (!is.null(folder)) {
    paste0(folder, "/", path)
  } else {
    path
  }
}


peer_file_place = function(repo_files, target_repo, input, message, branch, verbose, overwrite) {
  purrr::walk(
    input,
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
        cli::cli_alert_danger( paste(
          'Failed to add {.val {gh_path}} to {.val {target_repo}}: already exists.',
          'If you want to force add this file, re-run the command with {.code overwrite = TRUE}.'
        ) )
      }
    }
  )
}


local_path_content_grab = function(local_path = NULL, check_rmd = TRUE) {
  arg_is_chr(local_path, allow_null = TRUE)
  arg_is_lgl_scalar(check_rmd)

  purrr::map(
    local_path,
    function(local_path) {
      if (!is.null(local_path)) {
        file_status = fs::file_exists(local_path)
        if (!file_status)
          cli_stop("Unable to locate the following file: {.file {local_path}}")

        if (check_rmd & (tolower(fs::path_ext(local_path)) == "rmd")) {
          list(
            content = readChar(local_path, file.info(local_path)[['size']]),
            path = fs::path_file(local_path)
          )
        } else {
          cli_stop("{.field local_path} must be an {.file .Rmd} file.")
        }
      }
    }
  )
}


# subset repository files
repo_files_select = function(repo, repo_files = NULL, exclude_pattern, branch) {
  arg_is_chr_scalar(repo, branch)
  arg_is_chr(exclude_pattern, allow_null = T)

  if (is.null(repo_files)) {
    repo_files = repo_files(repo = repo, branch = branch)
  }

  sub = repo_files[['type']] == "blob" #& !grepl("/", repo_files[['path']])]
  path = repo_files[['path']][sub]

  rx_exclude_pattern = paste(utils::glob2rx(exclude_pattern), collapse = "|")
  path[!grepl(rx_exclude_pattern, path)]
}

format_commit_output = function(res = NULL, target_files = NULL, target_repo, target_path,
                                target_folder, category, changed = NA) {
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
    # mode = 100644 & type = "blob" is added manually to reduce API calls
    # mode = 100644 is the mode for a blob, see documentation
    # https://developer.github.com/v3/git/trees/#create-a-tree
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

peer_add_content = function(target_repo, target_folder, target_files, content,
                            content_compare = NULL, category, message, branch, overwrite) {
  arg_is_chr(target_repo)
  arg_is_chr_scalar(category, target_folder)
  arg_is_chr_scalar(message, branch, allow_null = TRUE)
  arg_is_lgl(overwrite)

  # TODO: Review to simplify nested mapping
  out = purrr::map_dfr(
    target_repo,
    function(r) {
      sub_r = target_files[target_files[['repo']] == r, ]

      purrr::map_dfr(
        content,
        function(c) {
          changed = NA

          target_path = paste0(target_folder, "/", c[['path']])

          target_exists = target_path %in% sub_r[['path']]

          if (target_exists &
              !overwrite) {
            cli::cli_alert_danger( paste(
                'Failed to add {.val {target_path}} to {.val {r}}: already exists.',
                'If you want to force add this file, re-run the command with {.code overwrite = TRUE}.'
            ) )

            format_commit_output(
              target_files = sub_r[sub_r[['path']] == target_path,],
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
                changed = !isTRUE(all.equal(c[['content']][1], content_compare[[n]][['content']][1]))
              }
            }

            if (is.na(changed) | changed) {
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
                target_files = sub_r[sub_r[['path']] == target_path,],
                target_repo = r,
                target_path = target_path,
                target_folder = target_folder,
                category = category,
                changed = changed
              )
            }
          }
        }
      )
    }
  )
  out

}

# Extract user from a repo of format org/repo
peer_repo_get_user = function(repo, org, prefix, suffix) {
  tmp = sub(glue::glue("^{org}/{prefix}(review-)?"), "", repo)
  tmp = sub(glue::glue("{suffix}(-review)?$"), "", tmp)
  tmp
}

# Grab content of specific files on a repo; return list w content and name
repo_path_content_grab = function(repo, path, repo_files = NULL, branch) {
  arg_is_chr_scalar(repo, branch)
  arg_is_chr(path)

  # TODO: Review to simplify nested mapping
  purrr::map(
    path,
    function(path) {
      if (is.null(repo_files)) {
        repo_files = repo_files(repo = repo, branch = branch)
      }

      path_exists = path %in% repo_files[['path']]

      if (path_exists) {
        res = purrr::safely(repo_get_file)(repo = repo,
                                           path = path,
                                           branch = branch)

        if (succeeded(res))
          list(content = res[['result']],
               path = path)
      } else {
        list(content = NULL,
             path = NULL)
      }
    }
  )
}

content_path_folder_strip = function(list, folder) {
  purrr::map(
    list,
    function(x) {
      purrr::modify_at(
        x, .at = c("path"),
        ~ gsub(paste0(folder, "/"), "", .x)
      )
    }
  )
}
