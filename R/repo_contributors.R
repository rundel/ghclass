github_api_repo_contributors = function(repo) {
  owner = get_repo_owner(repo)
  repo = get_repo_name(repo)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/stats/contributors",
    owner = owner,
    repo = repo
  )
}

#' @rdname repo_user
#' @export
#'
repo_contributors = function(repo) {

  arg_is_chr(repo)
  repo = unique(repo)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_contributors)(repo)

      status_msg(
        res,
        fail = "Failed to retrieve contributors for {.val {repo}}."
      )

      contribs = result(res)

      if (empty_result(contribs)) {
        d = tibble::tibble(
          repo = character(),
          username = character(),
          commits = integer(),
          weekly_stats = list()
        )
      } else {
        d = tibble::tibble(
          repo = repo,
          username = purrr::map_chr(contribs, c("author", "login")),
          commits = purrr::map_int(contribs, "total"),
          weekly_stats = purrr::map(
            contribs,
            function(cont) {
              dplyr::bind_rows(cont$weeks) %>%
              dplyr::transmute(
                week = lubridate::as_datetime(.data$w),
                additions = .data$a,
                deletions = .data$d,
                commits = .data$c
              )
            }
          )
        )
      }
    }
  )
}
