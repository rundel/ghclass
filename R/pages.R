#' @name pages
#' @rdname pages
#'
#' @title Retrieve information about GitHub Pages sites and builds.
#'
#' @description
#' * `pages_enabled()` - returns `TRUE` if a Pages site exists for the repo.
#'
#' * `pages_status()` - returns more detailed information about a repo's Pages site.
#'
#' * `pages_create()` - creates a Pages site for the provided repos.
#'
#' * `pages_delete()` - deletes the Pages site for the provided repos.
#'
#' @param repo Character. Address of repositories in `owner/name` format.
#'
#' @return
#'
#' `pages_enabled()` returns a named logical vector - `TRUE` if a Pages site exists, `FALSE` otherwise.
#'
#' `pages_status()` returns a tibble containing details on Pages sites.
#'
#' `pages_create()` & `pages_delete()` return an invisible list containing the API responses.
#'
#' @examples
#' \dontrun{
#' pages_enabled("rundel/ghclass")
#'
#' pages_status("rundel/ghclass")
#' }
#'
NULL


github_api_pages = function(repo) {
  arg_is_chr_scalar(repo)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/pages",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}



#' @name pages
#' @rdname pages
#'
#' @export
#'
pages_enabled = function(repo) {
  arg_is_chr(repo)

  purrr::map_lgl(
    repo,
    function(repo) {
      purrr::safely(github_api_pages)(repo) %>%
        succeeded()
    }
  ) %>%
    stats::setNames(repo)
}




#' @name pages
#' @rdname pages
#'
#' @export
#'
pages_status = function(repo) {
  arg_is_chr(repo)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_pages)(repo)

      status_msg(
        res,
        fail = "Failed find Pages information for repo {.val {repo}}."
      )

      if (failed(res) || empty_result(res)) {
        tibble::tibble(
          repo = character(),
          status = character(),
          url    = character(),
          build_type = character(),
          branch = character(),
          path   = character(),
          public = logical()
        )
      } else {
        page = list(result(res))
        tibble::tibble(
          repo   = repo,
          status = purrr::map_chr(page, "status", .default = NA),
          url    = purrr::map_chr(page, "html_url", .default = NA),
          build_type = purrr::map_chr(page, "build_type", .default = NA),
          branch = purrr::map_chr(page, c("source", "branch"), .default = NA),
          path   = purrr::map_chr(page, c("source", "path"), .default = NA),
          public = purrr::map_lgl(page, "public", .default = NA)
        )
      }
    }
  )
}




github_api_pages_create = function(repo, build_type, branch, path) {
  arg_is_chr_scalar(repo, build_type, branch, path)

  ghclass_api_v3_req(
    endpoint = "POST /repos/:owner/:repo/pages",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    build_type = build_type,
    source = list(
      branch = branch, path = path
    )
  )
}



#' @name pages
#' @rdname pages
#'
#' @param build_type Character. Either `"workflow"` or `"legacy"` - the former uses GitHub actions to
#' build and publish the site (requires a workflow file to achieve this).
#'
#' @param branch Character. Repository branch to publish.
#'
#' @param path Character. Repository path to publish.
#'
#' @export
#'
pages_create = function(
    repo,
    build_type = c("legacy", "workflow"),
    branch = "main",
    path = "/docs"
) {
  build_type = match.arg(build_type)
  arg_is_chr(repo)
  arg_is_chr_scalar(build_type, branch, path)

  purrr::map(
    repo,
    function(repo) {
      res = purrr::safely(github_api_pages_create)(repo, build_type, branch, path)

      status_msg(
        res,
        "Created Pages site for {.val {repo}}.",
        "Failed to create Pages site for repo {.val {repo}}."
      )

      res
    }
  ) %>%
    invisible()
}



github_api_pages_delete = function(repo) {
  arg_is_chr_scalar(repo)

  ghclass_api_v3_req(
    endpoint = "DELETE /repos/:owner/:repo/pages",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo)
  )
}



#' @name pages
#' @rdname pages
#'
#' @export
#'
pages_delete = function(repo) {
  arg_is_chr(repo)

  purrr::map(
    repo,
    function(repo) {
      res = purrr::safely(github_api_pages_delete)(repo)

      status_msg(
        res,
        "Deleted Pages site for repo {.val {repo}}.",
        "Failed to delete Pages site for repo {.val {repo}}."
      )

      res
    }
  ) %>%
    invisible()
}

