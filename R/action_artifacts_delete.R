github_api_delete_artifact = function(repo, id) {
  arg_is_chr_scalar(repo)
  arg_is_pos_int(id)

  ghclass_api_v3_req(
    endpoint = "DELETE /repos/{owner}/{repo}/actions/artifacts/{artifact_id}",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    artifact_id = id
  )

}


#' @name action
#' @rdname action
#'
#' @export
#'
action_artifact_delete = function(repo, ids) {

  arg_is_chr(repo)

  if (is.numeric(ids))
    ids = tibble::tibble(repo = repo, id = ids)
  arg_is_df(ids)

  df = dplyr::left_join(
    tibble::tibble(repo = repo), ids,
    by = "repo"
  ) %>%
    dplyr::select("repo", "id")

  purrr::pwalk(
    df,
    function(repo, id) {
      res = purrr::safely(github_api_delete_artifact)(repo, id)

      status_msg(
        res,
        "Deleted artifact {.val {id}} from repo {.val {repo}}.",
        "Failed to delete artifact with id {.val {id}} from repo {.val {repo}}."
      )
    }
  )

  invisible(df)
}
