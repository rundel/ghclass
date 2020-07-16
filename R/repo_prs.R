github_api_repo_prs = function(repo, state) {
  arg_is_chr_scalar(repo, state)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/pulls",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    state = state
  )
}


#' @rdname repo_details
#'
#' @param state Character. Pull request state.
#' @export
#'
repo_prs = function(repo, state = c("open","closed","all")) {
  state = match.arg(state)
  arg_is_chr(repo)
  arg_is_chr_scalar(state)



  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_prs)(repo, state)

      if (empty_result(res)) {
        tibble::tibble(
          repo = character(),
          pr = integer(),
          id = integer(),
          title = character(),
          state = character(),
          base_ref = character(),
          head_ref = character()
        )
      } else {
        tibble::tibble(
          repo = repo,
          pr = purrr::map_int(result(res), "number", .default = NA),
          id = purrr::map_int(result(res), "id", .default = NA),
          title = purrr::map_chr(result(res), "title", .default = NA),
          state = purrr::map_chr(result(res), "state", .default = NA),
          base_ref = purrr::map_chr(result(res), c("base","ref"), .default = NA),
          head_ref = purrr::map_chr(result(res), c("head","ref"), .default = NA)
        )
      }
    }
  )
}
