github_api_repo_tree = function(repo, sha = "master") {
  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo/git/trees/:sha?recursive=1",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    sha = sha
  )
}

repo_files = function(repo, branch = "master") {
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


file_exists = function(repo, path, branch = "master"){

  files = repo_files(repo, branch)

  purrr::map_lgl(path, ~.x %in% files[["path"]])

}



check_file_modification = function(repo, path, branch = "master"){
  arg_is_chr_scalar(repo, branch, path)
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


ghclass_api_v3_req = function(endpoint, ...) {
  method = endpoint_verb(endpoint)
  if (method == "GET") {
    limit = github_get_api_limit()
  } else { # Some non-GET methods don't like having a limit set
    limit = NULL
  }

  suppressMessages(
    gh::gh(
      endpoint = endpoint,
      ...,
      .limit = limit,
      .token = github_get_token()
      # .progress = FALSE # TODO - giving an error for some reason
    )
  )
}
