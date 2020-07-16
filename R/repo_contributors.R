github_api_repo_contributors = function(repo) {
  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/stats/contributors",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
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
      # Calculating these stats can take some time, initial request will begin the calculation
      # and return a 202, we wait n seconds and try again. Timeout after m seconds.
      res = purrr::safely(
        function() {
          # TODO - fix me, oh god it is terrible

          for(i in 1:10) {
            r = github_api_repo_contributors(repo)

            if (response_status(r) != "202 Accepted") {
              return(r)
            }

            Sys.sleep(1)
          }

          cli_stop("Timeout, no response within 10 seconds.")
        }
      )()

      status_msg(
        res,
        fail = "Failed to retrieve contributors for {.val {repo}}."
      )

      contribs = result(res)

      if (empty_result(contribs)) {
        d = tibble::tibble(
          repo = character(),
          username = character(),
          commits = integer()
        )
      } else {
        d = tibble::tibble(
          repo = repo,
          username = purrr::map_chr(contribs, c("author", "login")),
          commits = purrr::map_int(contribs, "total")
        )
      }
    }
  )
}
