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
#' @param keep_zip Logical. Should the downloaded zip files be retained (`TRUE`)
#'   or deleted after extraction (`FALSE`).
#' @param nest Logical. If `TRUE`, place each repo's artifacts under a
#'   `dir/{repo_name}/` subdirectory; otherwise flatten into `dir/` with
#'   repo-prefixed names.
#' @param overwrite Logical. Should existing zip files or extraction folders
#'   be overwritten.
#'
#' @export
#'
action_artifact_download = function(
  repo, dir,
  filter = NULL, exclude = FALSE,
  filter_branch = NULL, exclude_branch = FALSE,
  keep_zip = FALSE, nest = FALSE, overwrite = FALSE,
  ids = action_artifacts(
    repo,
    filter = filter, exclude = exclude,
    filter_branch = filter_branch, exclude_branch = exclude_branch
  )
) {
  arg_is_chr(repo)
  arg_is_chr_scalar(dir)
  arg_is_lgl_scalar(keep_zip, nest, overwrite, exclude_branch, exclude)
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

      repo_name = get_repo_name(cur_repo)

      purrr::map2_chr(
        cur_ids, cur_names,
        function(id, name) {
          if (nest) {
            repo_dir = fs::path_norm(fs::path(dir, repo_name))
            zip_path = fs::path_norm(fs::path(repo_dir, name, ext = "zip"))
            extract_dir = fs::path_norm(fs::path(repo_dir, name))
          } else {
            base = paste0(repo_name, "_", name)
            zip_path = fs::path_norm(fs::path(dir, base, ext = "zip"))
            extract_dir = fs::path_norm(fs::path(dir, base))
          }

          if ((file.exists(zip_path) || dir.exists(extract_dir)) && !overwrite) {
            cli::cli_alert_danger(
              paste0(
                "Destination {.file {extract_dir}} or {.file {zip_path}} already exists, ",
                "set {.code overwrite = TRUE} to overwrite."
              ),
              wrap = FALSE
            )
            return(NA_character_)
          }

          if (overwrite) {
            if (dir.exists(extract_dir))
              unlink(extract_dir, recursive = TRUE, force = TRUE)
            if (file.exists(zip_path))
              file.remove(zip_path)
          }

          if (nest)
            dir.create(fs::path_dir(zip_path), showWarnings = FALSE, recursive = TRUE)

          dl = purrr::safely(github_api_download_artifact)(cur_repo, id, dest = zip_path)
          if (failed(dl)) {
            cli::cli_alert_danger(
              "Failed to download artifact with id {.val {id}} from repo {.val {cur_repo}}.",
              wrap = FALSE
            )
            return(NA_character_)
          }

          dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)
          unzipped = purrr::safely(utils::unzip)(zip_path, exdir = extract_dir)
          if (failed(unzipped)) {
            cli::cli_alert_danger(
              "Failed to extract artifact with id {.val {id}} from repo {.val {cur_repo}} to {.file {extract_dir}}.",
              wrap = FALSE
            )
            return(NA_character_)
          }

          if (!keep_zip)
            file.remove(zip_path)

          cli::cli_alert_success(
            "Downloaded artifact {.val {id}} from repo {.val {cur_repo}} to {.file {extract_dir}}.",
            wrap = FALSE
          )

          extract_dir
        }
      )
    }
  )

  invisible(unlist(res))
}
