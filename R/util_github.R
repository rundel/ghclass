github_api_repo_get_tree = function(repo, sha = "master") {
  gh::gh(
    "GET /repos/:owner/:repo/git/trees/:sha?recursive=1",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    sha = sha,
    .token = github_get_token()
  )
}

repo_files = function(repo, branch = "master") {
  purrr::map2_dfr(
    repo, branch,
    function(repo, branch) {
      res = purrr::safely(github_api_repo_get_tree)(repo, branch)

      if (failed(res)) {
        r = usethis::ui_value(format_repo(repo, branch))
        usethis::ui_oops("Failed to retrieve files for repo {r}")
        return(NULL)
      }

      purrr::map_dfr(result(res)[["tree"]], ~c(repo = repo, .))
    }
  )
}

github_api_org_accept_invite = function(org, token) {
  arg_is_chr_scalar(org, token)

  gh::gh(
    "PATCH /user/memberships/orgs/:org",
    org = org,
    state = "active",
    .token = token
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
        glue::glue("Accepted {usethis::ui_value(user)}s invite to org {usethis::ui_value(org)}."),
        glue::glue("Failed to accept {usethis::ui_value(user)}s invite to org {usethis::ui_value(org)}.")
      )
    }
  )
}


# Extracts base64 encoded content from files
extract_content = function(file, include_details = TRUE) {
  if (is.null(file))
    return(NULL)

  content = base64enc::base64decode(file[["content"]])
  content = purrr::possibly(rawToChar, content)(content)

  if (include_details) {
    file[["content"]] = NULL
    attributes(content) = file
  }

  content
}

github_api_code_search = function(query) {
  gh::gh("GET /search/code", q = query,
         .token = github_get_token(),
         .limit = github_get_api_limit())
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
          usethis::ui_oops("Cannot find file {usethis::ui_value(file)} on {usethis::ui_value(repo)}.")
        }
      }
    )
  )
}


file_exists = function(repo, path, branch = "master"){

  files = repo_files(repo, branch)

  purrr::map_lgl(path, ~.x %in% files[["path"]])

}

github_api_get_commits = function(repo, sha=NULL, path=NULL, author=NULL, since=NULL, until=NULL) {
  args = list(
    endpoint = "GET /repos/:owner/:repo/commits",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token()
  )

  args[["sha"]] = sha
  args[["path"]] = path
  args[["author"]] = author
  args[["since"]] = since
  args[["until"]] = until

  do.call(gh::gh, args)
}


get_commits = function(repo, sha = NULL, path = NULL, author = NULL, since = NULL, until = NULL) {

  arg_is_chr(repo)
  arg_is_chr_scalar(repo, sha, path, author, since, until, allow_null = TRUE)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_get_commits)(
        repo, sha, path, author, since, until
      )

      # API gives an error if the repo has 0 commits
      res = allow_error(res, message = "Git Repository is empty")

      status_msg(
        res,
        fail = glue::glue("Failed to retrieve commits from {usethis::ui_value(repo)}.")
      )

      commits = result(res)

      if (empty_result(commits)) {
        tibble::tibble(
          repo = character(),
          path = character(),
          sha  = character(),
          user = character(),
          date = character(),
          msg  = character()
        )
      } else {
        tibble::tibble(
          repo = repo,
          path = ifelse(!is.null(path), path, character()),
          sha  = purrr::map_chr(commits, "sha"),
          user = purrr::map_chr(commits, c("author","login")),
          date = purrr::map_chr(commits, c("commit","author","date")),
          msg  = purrr::map_chr(commits, c("commit","message"))
        )
      }
    }
  )
}

check_file_modification = function(repo, path, branch = "master"){
  arg_is_chr_scalar(repo, branch, path)
  commits = get_commits(repo, sha = branch, path = path)
  nrow(commits) > 1
}
