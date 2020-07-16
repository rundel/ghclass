github_api_repo_issues = function(
  repo,
  state	= c("open", "closed", "all"),
  assignee = NULL,
  creator	= NULL,
  mentioned	= NULL,
  labels = NULL,
  sort = c("created", "updated", "comments"),
  direction =	c("desc", "asc"),
  since	= NULL
) {
  state = match.arg(state)
  sort = match.arg(sort)
  direction = match.arg(direction)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/issues",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    state = state,
    assignee = assignee,
    creator = creator,
    mentioned = mentioned,
    labels = labels,
    sort = sort,
    direction = direction,
    since = since
  )
}

#' @rdname repo_details
#'
#' @param state	    Character. State of the issues to return. Can be either "open", "closed", or "all".
#' @param assignee	Character. Return issues assigned to a particular username.
#'   Pass in "none" for issues with no assigned user, and "*" for issues assigned to any user.
#' @param creator	  Character. Return issues created the by the given username.
#' @param mentioned Character. Return issues that mentioned the given username.
#' @param labels	  Character. Return issues labeled with one or more of of the given label names.
#' @param sort	    Character. What to sort results by. Can be either "created", "updated", or "comments".
#' @param direction Character. The direction of the sort. Can be either "asc" or "desc".
#' @param since	    Character. Only issues updated at or after this time are returned.
#'
#' @export
#'
repo_issues = function(
  repo,
  state	= c("open", "closed", "all"),
  assignee = NULL,
  creator	= NULL,
  mentioned	= NULL,
  labels = NULL,
  sort = c("created", "updated", "comments"),
  direction =	c("desc", "asc"),
  since	= NULL
) {
  arg_is_chr(repo)
  arg_is_chr(labels, allow_null=TRUE)
  arg_is_chr_scalar(assignee, creator, mentioned, since, allow_null=TRUE)

  state = match.arg(state)
  sort = match.arg(sort)
  direction = match.arg(direction)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_issues)(
        repo = repo,
        state = state, assignee = assignee, creator = creator,
        mentioned = mentioned, labels = labels, sort = sort,
        direction = direction, since = since
      )

      status_msg(
        res,
        "Retrieved issues for repo {.val {repo}}.",
        "Failed to retrieve issues for repo {.val {repo}}."
      )

      if (succeeded(res)) {
        iss = result(res)

        tibble::tibble(
          repo = repo,
          number = purrr::map_int(iss, "number"),
          title = purrr::map_chr(iss, "title"),
          state = purrr::map_chr(iss, "state"),
          comments = purrr::map_int(iss, "comments"),
          created = purrr::map_chr(iss, "created_at") %>% lubridate::ymd_hms(),
          updated = purrr::map_chr(iss, "updated_at") %>% lubridate::ymd_hms(),
          closed = purrr::map_chr(iss, "closed_at", .default = NA) %>% lubridate::ymd_hms(),
          created_by = purrr::map_chr(iss, c("user","login")),
          assignees = purrr::map(iss, ~ purrr::map_chr(.x$assignees, "login"))
        )
      } else {
        list()
      }
    }
  )
}
