github_api_repo_contributors = function(repo, timeout = 10, wait = 0.5) {
  owner = get_repo_owner(repo)
  repo = get_repo_name(repo)

  make_req = function() {
    ghclass_api_v3_req(
      endpoint = "GET /repos/:owner/:repo/stats/contributors",
      owner = owner,
      repo = repo
    )
  }

  start = Sys.time()

  # Calculating these stats can take some time, initial request will begin the calculation
  # and return a 202, we wait n seconds and try again. Timeout after m seconds.

  while(difftime(Sys.time(), start, units = "secs") < timeout) {
    r = make_req()

    if (response_status(r) == "200 OK") {
      return(r)
    }

    Sys.sleep(wait)
  }

  cli_stop("Timeout - no response within {timeout} seconds.")
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
                week = lubridate::as_datetime(w),
                additions = a,
                deletions = d,
                commits = c
              )
            }
          )
        )
      }
    }
  )
}
