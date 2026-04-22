github_api_download_artifact = function(repo, id, dest) {
  id = as.double(id)
  arg_is_chr_scalar(repo)
  #arg_is_pos_int(id)

  archive_format = "zip"
  if (missing(dest))
    dest =  tempfile(fileext = paste0(".", archive_format))

  owner = get_repo_owner(repo)
  repo = get_repo_name(repo)

  url = glue::glue("https://api.github.com/repos/{owner}/{repo}/actions/artifacts/{id}/{archive_format}")

  github_api_download_file(url, dest)
}


#' @name action
#' @rdname action
#'
#' @param keep_zip Logical. Should the artifact zips be saved (`TRUE`) or their contents (`FALSE`).
#' @param file_pat Character. If extracting zip with multiple files, regexp pattern to match filename.
#' @param overwrite Logical. Should existing files be overwritten.
#'
#' @export
#'
action_artifact_download = function(
  repo, dir, 
  filter = NULL, exclude = FALSE,
  filter_branch = NULL, exclude_branch = FALSE,
  keep_zip = FALSE, file_pat = "", overwrite = FALSE,
  ids = action_artifacts(
    repo,
    filter = filter, exclude = exclude,
    filter_branch = filter_branch, exclude_branch = exclude_branch
  )
) {
  arg_is_chr(repo)
  arg_is_chr_scalar(dir, file_pat)
  arg_is_lgl_scalar(keep_zip, exclude_branch, exclude)
  arg_is_chr_scalar(filter_branch, allow_null = TRUE)
  arg_is_chr_scalar(filter, allow_null = TRUE)


  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  if (is.numeric(ids))
    ids = tibble::tibble(repo = repo, id = ids, name = as.character(ids))
  arg_is_df(ids, allow_empty = TRUE)

  if (nrow(ids) == 0) {
    cli::cli_alert_danger("No artifacts available for the given repos.")
    return(invisible(character()))
  }

  df = dplyr::left_join(
    tibble::tibble(repo = repo),
    ids,
    by = "repo"
  ) %>%
    dplyr::select("repo", "id", "name")

  repo_groups = split(df, df[["repo"]])

  res = purrr::map(
    repo_groups,
    function(repo_df) {
      cur_repo = repo_df[["repo"]][1]
      cur_ids = repo_df[["id"]]
      cur_names = repo_df[["name"]]

      if (all(is.na(cur_ids))) {
        cli::cli_alert_danger(
          "No artifacts found for repo {.val {cur_repo}}.",
          wrap = FALSE
        )
        return(NA_character_)
      }

      not_na = !is.na(cur_ids)
      cur_ids = cur_ids[not_na]
      cur_names = cur_names[not_na]

      downloads = purrr::map2(
        cur_ids, cur_names,
        function(id, name) {
          dl = purrr::safely(github_api_download_artifact)(cur_repo, id)
          if (failed(dl)) {
            cli::cli_alert_danger(
              "Failed to download artifact with id {.val {id}} from repo {.val {cur_repo}}.",
              wrap = FALSE
            )
          }
          list(id = id, name = name, res = dl)
        }
      )

      if (keep_zip) {
        use_name_suffix = sum(purrr::map_lgl(downloads, function(dl) !failed(dl[["res"]]))) > 1

        res = purrr::map_chr(downloads, function(dl) {
          if (failed(dl[["res"]])) return(NA_character_)

          id = dl[["id"]]
          name = dl[["name"]]
          file = result(dl[["res"]])

          if (use_name_suffix) {
            dest_path = fs::path_norm(glue::glue("{dir}/{get_repo_name(cur_repo)}_{name}.zip"))
          } else {
            dest_path = fs::path_norm(glue::glue("{dir}/{get_repo_name(cur_repo)}.zip"))
          }

          if (file.exists(dest_path) & !overwrite) {
            cli::cli_alert_danger(
              "File {.file {dest_path}} already exists, set {.code overwrite = TRUE} to overwrite this file.",
              wrap = FALSE
            )
            return(NA_character_)
          }

          file.rename(file, dest_path)

          cli::cli_alert_success(
            "Downloaded artifact {.val {id}} from repo {.val {cur_repo}} to {.val {dest_path}}.",
            wrap = FALSE
          )
          dest_path
        })

        return(res)
      }

      matches = purrr::map(downloads, function(dl) {
        if (failed(dl[["res"]])) return(NULL)

        file = result(dl[["res"]])
        art_files = utils::unzip(file, list = TRUE)[["Name"]]
        matched = art_files[grepl(file_pat, art_files)]

        if (length(matched) == 0) return(NULL)
        if (length(matched) != 1) {
          cli::cli_alert_danger(
            paste0(
              "Downloaded artifact with id {.val {dl[['id']]}} from repo {.val {cur_repo}} contains ",
              "{.val {length(matched)}} files matching {.val {file_pat}}.",
              "\nMatching files: {.val {matched}}."
            ),
            wrap = FALSE
          )
          return(NULL)
        }

        list(id = dl[["id"]], name = dl[["name"]], zip = file, art_file = matched)
      })

      matches = purrr::compact(matches)

      if (length(matches) == 0) {
        all_ids = cur_ids
        cli::cli_alert_danger(
          "No artifacts from repo {.val {cur_repo}} (ids: {.val {all_ids}}) contain files matching {.val {file_pat}}.",
          wrap = FALSE
        )
        return(NA_character_)
      }

      use_name_suffix = length(matches) > 1

      purrr::map_chr(matches, function(m) {
        ext = fs::path_ext(m[["art_file"]])
        if (ext != "") ext = paste0(".", ext)

        if (use_name_suffix) {
          dest_path = fs::path_norm(glue::glue("{dir}/{get_repo_name(cur_repo)}_{m[['name']]}{ext}"))
        } else {
          dest_path = fs::path_norm(glue::glue("{dir}/{get_repo_name(cur_repo)}{ext}"))
        }

        if (file.exists(dest_path) & !overwrite) {
          cli::cli_alert_danger(
            "File {.file {dest_path}} already exists, set {.code overwrite = TRUE} to overwrite this file.",
            wrap = FALSE
          )
          return(NA_character_)
        }

        data = readr::read_file_raw(unz(m[["zip"]], m[["art_file"]]))
        readr::write_file(data, dest_path)

        cli::cli_alert_success(
          "Downloaded artifact {.val {m[['id']]}} from repo {.val {cur_repo}} to {.val {dest_path}}.",
          wrap = FALSE
        )
        dest_path
      })
    }
  )

  invisible(unlist(res))
}
