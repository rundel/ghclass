github_api_download_file = function(url, dest) {
  arg_is_chr_scalar(url, dest)

  req = httr::GET(
    url,
    httr::add_headers(
      Authorization = paste("bearer", github_get_token())
    )
  )

  res = httr::content(req, as = "raw")
  code = httr::status_code(req)

  if (code >= 300) {
    cli_stop("GitHub API Error ({code}) - {res[['message']]}")
  }

  writeBin(res, dest)

  return(dest)
}

github_api_repo_tree = function(repo, sha = NULL) {
  arg_is_chr_scalar(repo)
  arg_is_chr_scalar(sha, allow_null = TRUE)

  if (is.null(sha))
    sha = "HEAD"

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/git/trees/:sha?recursive=1",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    sha = sha
  )
}

repo_files = function(repo, branch = NULL) {
  arg_is_chr(repo)
  arg_is_chr(branch, allow_null = TRUE)

  if (is.null(branch))
    branch = list(NULL)

  purrr::map2_dfr(
    repo, branch,
    function(repo, branch) {

      res = purrr::safely(github_api_repo_tree)(repo, branch)
      repo_txt = format_repo(repo, branch)

      if (failed(res)) {
        cli::cli_alert_danger("Failed to retrieve files for repo {.val {repo_txt}}")
        return(NULL)
      }

      purrr::map_dfr(result(res)[["tree"]], ~c(repo = repo, .))
    }
  )
}

github_api_org_accept_invite = function(org, token) {
  arg_is_chr_scalar(org, token)

  with_pat(
    token,
    ghclass_api_v3_req(
      endpoint = "PATCH /user/memberships/orgs/:org",
      org = org,
      state = "active"
    )
  )
}

org_accept_invite = function(org, user, pat) {
  arg_is_chr(org, pat)

  purrr::pwalk(
    list(org, user, pat),
    function(org, user, pat) {
      res = purrr::safely(github_api_org_accept_invite)(org, pat)

      status_msg(
        res,
        "Accepted {.val {user}}s invite to org {.val {org}}.",
        "Failed to accept {.val {user}}s invite to org {.val {org}}."
      )
    }
  )
}

# Extracts base64 encoded content from files
extract_content = function(repo, path, file, include_details = TRUE, quiet = FALSE) {
  if (is.null(file) | is.null(file[["content"]])) {
    if (!quiet)
      cli::cli_alert_danger(
        "Unable to retrieve file {.val {path}} from repo {.val {repo}}."
      )
    return(invisible(NULL))
  }

  content = base64enc::base64decode(file[["content"]])
  content = purrr::possibly(rawToChar, content)(content)

  if (include_details) {
    file[["content"]] = NULL
    attributes(content) = file
  }

  content
}

github_api_code_search = function(query) {
  ghclass_api_v3_req(
    endpoint = "GET /search/code",
    q = query
  )
}


find_file = function(repo, file, verbose = TRUE){
  arg_is_chr_scalar(repo)
  arg_is_chr(file)

  purrr::flatten_chr(
    purrr::map(
      file,
      function(file) {

        query = paste0(" path:", ifelse(!(fs::path_dir(file) == "."), fs::path_dir(file), "/"),
                       " repo:", repo,
                       " filename:", fs::path_file(file))
        res = github_api_code_search(query)

        if(res[["total_count"]] > 0){
          purrr::map_chr(res[["items"]], "path")
        } else if (verbose){
          cli::cli_alert_danger("Cannot find file {.val {file}} on {.val {repo}}.")
        }
      }
    )
  )
}


file_exists = function(repo, path, branch = NULL){
  arg_is_chr(repo, path)
  arg_is_chr(branch, allow_null = TRUE)

  files = repo_files(repo, branch)

  purrr::map_lgl(path, ~.x %in% files[["path"]])

}



check_file_modification = function(repo, path, branch){
  arg_is_chr_scalar(repo, path, branch)
  commits = repo_commits(repo, branch = branch, path = path)
  nrow(commits) > 1
}


response_status = function(x) {
  attr(x, "response")[["status"]]
}

# Modified from gh:::gh_set_verb
endpoint_verb = function(x) {
  if (!nzchar(x))
    return(NA)

  if (grepl("^/", x) || grepl("^http", x)) {
     method = "GET"
  } else {
    method = gsub("^([^/ ]+)\\s+.*$", "\\1", x)
  }

  stopifnot(method %in% c("GET", "POST", "PATCH", "PUT", "DELETE"))

  method
}


ghclass_api_v3_req = function(
    endpoint, ..., .send_headers = character(),
    version = "2022-11-28", limit = github_get_api_limit()
) {
  arg_is_chr_scalar(version)
  arg_is_chr(.send_headers)

  method = endpoint_verb(endpoint)

  # Support for GitHub API versioning
  # https://github.blog/2022-11-28-to-infinity-and-beyond-enabling-the-future-of-githubs-rest-api-with-api-versioning/
  .send_headers["X-GitHub-Api-Version"] = version

  if (method != "GET") { # Some non-GET methods don't like having a limit set
    limit = NULL
  }

  res = suppressMessages(
    gh::gh(
      endpoint = endpoint,
      ...,
      .limit = limit,
      .token = github_get_token(),
      .send_headers = .send_headers
      # .progress = FALSE # TODO - giving an error for some reason
    )
  )

  if (length(res) == github_get_api_limit()) {
    cli::cli_warn(
      c("The number of results is equal to the limit set by {.fn github_set_api_limit},",
        "consider increasing this limit and rerunning the previous function.")
    )
  }

  res
}
