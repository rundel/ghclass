github_api_repo_contributors = function(repo, max_retries = 5, retry_delay = 2) {
  owner = get_repo_owner(repo)
  name  = get_repo_name(repo)

  url = paste0(github_api_url(), "/repos/", owner, "/", name, "/stats/contributors")

  for (i in seq_len(max_retries + 1)) {
    req = httr::GET(
      url,
      httr::add_headers(
        Authorization = paste("bearer", github_get_token()),
        "X-GitHub-Api-Version" = "2022-11-28",
        Accept = "application/vnd.github+json"
      )
    )

    code = httr::status_code(req)

    if (code == 202) {
      if (i <= max_retries) {
        Sys.sleep(retry_delay)
        next
      }
      cli::cli_warn(
        "GitHub is still computing contributor stats for {.val {repo}} after {max_retries} retries; try again shortly."
      )
      return(list())
    }

    if (code >= 300) {
      content = httr::content(req)
      cli_stop("GitHub API Error ({code}) - {content$message}")
    }

    return(httr::content(req))
  }
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
