github_api_download_artifact = function(repo, id, dest) {
  id = as.integer(id)
  arg_is_chr_scalar(repo)
  arg_is_pos_int(id)

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
  repo, dir, ids = action_artifacts(repo),
  keep_zip=FALSE, file_pat = "", overwrite = FALSE
) {
  arg_is_chr(repo)
  arg_is_chr_scalar(dir, file_pat)
  arg_is_lgl_scalar(keep_zip)


  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  if (is.numeric(ids))
    ids = tibble::tibble(repo = repo, id = ids)
  arg_is_df(ids)

  if (nrow(ids) == 0)
    cli_stop("No artifacts available for the given repos.")

  df = dplyr::left_join(
    tibble::tibble(repo = repo),
    ids,
    by = "repo"
  ) %>%
    dplyr::select("repo", "id")

  res = purrr::pmap_chr(
    df,
    function(repo, id) {
      res = purrr::safely(github_api_download_artifact)(repo, id)
      file = result(res)

      if (failed(res)) {
        cli::cli_alert_danger(
          "Failed to download artifact with id {.val {id}} from repo {.val {repo}}.",
          wrap = FALSE
        )
        return(NA_character_)
      }

      if (keep_zip) {
        dest_path = fs::path_norm( glue::glue("{dir}/{get_repo_name(repo)}.zip") )

        if (file.exists(dest_path) & !overwrite) {
          cli::cli_alert_danger(
            "File {.file {dest_path}} already exists, set {.code overwrite = TRUE} to overwrite this file.",
            wrap = FALSE
          )
          return(NA_character_)
        }

        file.rename(file, dest_path)
      } else {
        art_file = utils::unzip(file, list=TRUE)[["Name"]]
        art_file = art_file[grepl(file_pat, art_file)]

        if (length(art_file) != 1) {
          cli::cli_alert_danger(
            paste0("Downloaded artifact with id {.val {id}} from repo {.val {repo}} contains ",
                  "{.val {length(art_file)}} files matching {.val {file_pat}}.",
                  ifelse(length(art_file), "\nMatching files: {.val {art_file}}.", "")),
            wrap = FALSE
          )
          return(NA_character_)
        }
        ext = fs::path_ext(art_file)
        if (ext != "") ext = paste0(".", ext)

        data = readr::read_file_raw(unz(file, art_file))

        dest_path = fs::path_norm( glue::glue("{dir}/{get_repo_name(repo)}{ext}") )

        if (file.exists(dest_path) & !overwrite) {
          cli::cli_alert_danger(
            "File {.file {dest_path}} already exists, set {.code overwrite = TRUE} to overwrite this file.",
            wrap = FALSE
          )
          return(NA_character_)
        }

        readr::write_file(data, dest_path)
      }

      status_msg(
        res,
        "Downloaded artifact {.val {id}} from repo {.val {repo}} to {.val {dest_path}}.",
        NULL
      )

      dest_path
    }
  )

  invisible(res)
}
