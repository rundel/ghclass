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
#' @details
#' For the `"legacy"` build type the branch being published must already exist, otherwise
#' `pages_create()` fails. GitHub enables Pages automatically when a branch named `gh-pages`
#' is created (e.g. with [branch_create()]), publishing that branch from `/`, so
#' `pages_create()` is not needed in that case.
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

  status_scope(
    "Retrieving Pages status", length(repo),
    done = "Retrieved Pages status for {n_ok} of {total} repo{?s}",
    purrr::map_dfr(
      repo,
      function(repo) {
        res = purrr::safely(github_api_pages)(repo)

        status_msg(
          res,
          fail = "Failed to retrieve Pages status for repo {.val {repo}}."
        )

        if (failed(res) || empty_result(res)) {
          tibble::tibble(
            repo = character(),
            status = character(),
            url    = character(),
            build_type = character(),
            branch = character(),
            path   = character(),
            public = logical(),
            cname  = character(),
            custom_404 = logical(),
            https_enforced = logical()
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
            public = purrr::map_lgl(page, "public", .default = NA),
            cname  = purrr::map_chr(page, "cname", .default = NA),
            custom_404 = purrr::map_lgl(page, "custom_404", .default = NA),
            https_enforced = purrr::map_lgl(page, "https_enforced", .default = NA)
          )
        }
      }
    )
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



# GitHub enables Pages on its own when a `gh-pages` branch is created
pages_already_enabled = function(res) {
  e = error(res)

  inherits(e, "http_error_409") &&
    grepl(
      "already enabled",
      paste(c(e[["message"]], e[["response_content"]][["message"]]), collapse = " "),
      fixed = TRUE
    )
}

pages_site_matches = function(site, build_type, branch, path) {
  if (!identical(site[["build_type"]], build_type))
    return(FALSE)

  build_type == "workflow" ||
    (identical(site[["source"]][["branch"]], branch) && identical(site[["source"]][["path"]], path))
}



#' @name pages
#' @rdname pages
#'
#' @param build_type Character. Either `"workflow"` or `"legacy"` - the former uses GitHub actions to
#' build and publish the site (requires a workflow file to achieve this).
#'
#' @param branch Character. Repository branch to publish, which must already exist.
#'
#' @param path Character. Repository path to publish.
#'
#' @export
#'
pages_create = function(
    repo,
    build_type = c("legacy", "workflow"),
    branch = "main",
    path = "/"
) {
  build_type = match.arg(build_type)

  arg_is_chr(repo)
  arg_is_chr_scalar(build_type, branch, path)

  res = status_scope(
    "Creating Pages sites", length(repo),
    done = "Created Pages sites for {n_ok} of {total} repo{?s}",
    purrr::map(
      repo,
      function(repo) {
        res = purrr::safely(github_api_pages_create)(repo, build_type, branch, path)

        if (pages_already_enabled(res)) {
          cur = purrr::safely(github_api_pages)(repo)

          if (succeeded(cur) && pages_site_matches(result(cur), build_type, branch, path)) {
            status_skip("Skipping Pages site for repo {.val {repo}}, it already exists.")
            return(cur)
          }
        }

        status_msg(
          res,
          "Created Pages site for repo {.val {repo}}.",
          "Failed to create Pages site for repo {.val {repo}}."
        )

        res
      }
    )
  )

  invisible(res)
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
#' @param prompt Logical. Should the user be prompted before deleting Pages sites. Default `TRUE`.
#'
#' @export
#'
pages_delete = function(repo, prompt = TRUE) {
  arg_is_chr(repo)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = cli_yeah("This command will delete Pages sites for the following repositories permanently: {.val {repo}}.")
    if (!delete) {
      return(invisible())
    }
  }

  res = status_scope(
    "Deleting Pages sites", length(repo),
    done = "Deleted Pages sites for {n_ok} of {total} repo{?s}",
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
    )
  )

  invisible(res)
}

