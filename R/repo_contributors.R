github_api_repo_contributors = function(repo) {
  gh::gh(
    "GET /repos/:owner/:repo/stats/contributors",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token(),
    .limit = github_get_api_limit()
  )
}

#' Get repository contributor(s)
#'
#' `repo_contributors` Returns a data frame containing statistics on contributor(s) to the provided repositories.
#'
#' @param repo Character. Address of one or more repositories in `owner/name` format.
#' @param include_admins Logical. If `FALSE`, usernames of users with Admin rights are excluded, defaults to `TRUE`.
#'
#' @return A tibble.
#'
#' @examples
#' \dontrun{
#' repo_contributors("ghclass-test/test")
#' }
#'
#' @export
#'
repo_contributors = function(repo, include_admins = TRUE) {

  arg_is_chr(repo)
  repo = unique(repo)

  admins = character()

  if (!include_admins) {
    org = unique(get_repo_owner(repo))
    stopifnot(length(org) == 1)
    admins = org_admins(org)
  }

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

      d[!d[["username"]] %in% admins, ]
    }
  )
}
