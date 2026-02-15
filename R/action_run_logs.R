github_api_download_run_logs = function(repo, run_id, dest) {
  arg_is_chr_scalar(repo)
  run_id = as.double(run_id)

  if (missing(dest))
    dest = tempfile(fileext = ".zip")

  ghclass_api_v3_req(
    endpoint = "GET /repos/{owner}/{repo}/actions/runs/{run_id}/logs",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    run_id = run_id,
    .destfile = dest,
    .overwrite = TRUE
  )

  dest
}


#' @name action
#' @rdname action
#'
#' @param run_ids Integer or data frame. Run ids for which to download logs.
#' If a data frame is passed then the `run_id` column will be used.
#' Defaults to the most recent run for each repo (i.e. `action_runs(repo)`).
#' @param keep_zip Logical. Should the log zips be saved (`TRUE`) or their contents extracted (`FALSE`).
#' @param overwrite Logical. Should existing files be overwritten.
#'
#' @export
#'
action_run_logs = function(
  repo, dir, run_ids = action_runs(repo),
  keep_zip = FALSE, overwrite = FALSE
) {
  arg_is_chr(repo)
  arg_is_chr_scalar(dir)
  arg_is_lgl_scalar(keep_zip, overwrite)

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  if (is.numeric(run_ids))
    run_ids = tibble::tibble(repo = repo, run_id = run_ids)
  arg_is_df(run_ids)

  if (nrow(run_ids) == 0)
    cli_stop("No action runs available for the given repos.")

  df = dplyr::left_join(
    tibble::tibble(repo = repo),
    run_ids,
    by = "repo"
  ) %>%
    dplyr::select("repo", "run_id")

  res = purrr::pmap_chr(
    df,
    function(repo, run_id) {
      res = purrr::safely(github_api_download_run_logs)(repo, run_id)
      file = result(res)

      if (failed(res)) {
        cli::cli_alert_danger(
          "Failed to download logs for run {.val {run_id}} from repo {.val {repo}}.",
          wrap = FALSE
        )
        return(NA_character_)
      }

      if (keep_zip) {
        dest_path = fs::path_norm(glue::glue("{dir}/{get_repo_name(repo)}_{run_id}.zip"))

        if (file.exists(dest_path) & !overwrite) {
          cli::cli_alert_danger(
            "File {.file {dest_path}} already exists, set {.code overwrite = TRUE} to overwrite this file.",
            wrap = FALSE
          )
          return(NA_character_)
        }

        file.rename(file, dest_path)
      } else {
        dest_path = fs::path_norm(glue::glue("{dir}/{get_repo_name(repo)}_{run_id}"))

        if (dir.exists(dest_path) & !overwrite) {
          cli::cli_alert_danger(
            "Directory {.file {dest_path}} already exists, set {.code overwrite = TRUE} to overwrite.",
            wrap = FALSE
          )
          return(NA_character_)
        }

        dir.create(dest_path, showWarnings = FALSE, recursive = TRUE)
        utils::unzip(file, exdir = dest_path, overwrite = overwrite)
      }

      status_msg(
        res,
        "Downloaded logs for run {.val {run_id}} from repo {.val {repo}} to {.val {dest_path}}.",
        NULL
      )

      dest_path
    }
  )

  invisible(res)
}
