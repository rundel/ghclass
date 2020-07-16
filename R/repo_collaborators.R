github_api_repo_collaborators = function(repo) {
  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/collaborators",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}


#' @rdname repo_user
#' @export
#'
repo_collaborators = function(repo, include_admins = TRUE) {
  arg_is_chr(repo)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_collaborators)(repo)
      status_msg(
        res,
        fail = "Failed to retrieve collaborators for {.val {repo}}."
      )

      collabs = result(res)

      d = if (empty_result(collabs)) {
        tibble::tibble(
          repo = character(),
          username = character(),
          pull  = logical(),
          push  = logical(),
          admin = logical()
        )
      } else {
        tibble::tibble(
          repo = repo,
          username = purrr::map_chr(collabs, "login"),
          pull  = purrr::map_lgl(collabs, c("permissions", "pull")),
          push  = purrr::map_lgl(collabs, c("permissions", "push")),
          admin = purrr::map_lgl(collabs, c("permissions", "admin"))
        )
      }

      if (!include_admins)
        d = dplyr::filter(d, !.data$admin)

      d
    }
  )
}
