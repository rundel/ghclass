extract_content = function(file, include_details = TRUE) {
  if (is.null(file))
    return(NULL)

  content = base64enc::base64decode(file[["content"]])
  content = rawToChar(content)

  if (include_details) {
    file[["content"]] = NULL
    attributes(content) = file
  }

  content
}

github_api_get_readme = function(repo, branch) {
  stopifnot(length(repo) == 1)
  stopifnot(length(branch) == 1)

  name = get_repo_name(repo)
  owner = get_repo_owner(repo)

  gh::gh(
    "GET /repos/:owner/:repo/readme",
    owner = owner, repo = name, ref = branch,
    .token = get_github_token(), .limit = get_github_api_limit()
  )
}

#' Low level function for retrieving the README of a GitHub Repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param branch Character. Name of branch to use, defaults to "master".
#'
#' @family file functions
#'
#' @export
#'
get_readme = function(repo, branch = "master") {
  stopifnot(length(repo) == 1)
  stopifnot(length(branch) == 1)

  file = purrr::possibly(github_api_get_readme, NULL)(repo, branch)

  extract_content(file)
}

github_api_get_file = function(repo, file, branch) {

  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)
  stopifnot(length(branch) == 1)

  name = get_repo_name(repo)
  owner = get_repo_owner(repo)

  gh::gh(
    "GET /repos/:owner/:repo/contents/:path",
    owner = owner, repo = name, path = file, ref = branch,
    .token = get_github_token(), .limit = get_github_api_limit()
  )

}


#' Low level function for retrieving a file from a GitHub Repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param file Characer. Path to the file within the repository.
#' @param branch Character. Name of branch to use, defaults to "master".
#'
#' @family file functions
#'
#' @export
#'
get_file = function(repo, file, branch = "master") {
  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)
  stopifnot(length(branch) == 1)

  file = purrr::possibly(github_api_get_file, NULL)(repo, file, branch)

  extract_content(file)
}

#' Modify a file within a repository
#'
#' @param repo Character. Address of repository in `owner/name`` format.
#' @param file Character. File's path within the repository.
#' @param content Character. Content to be added to the file.
#' @param after Character. Regex pattern, if not `NULL` content will be inserted directly after the first match.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch to use, defaults to "master".
#'
#' @family file functions
#'
#' @export
#'
add_content = function(repo, file, content, after = NULL, message = "Added content", branch = "master") {
  arg_is_chr(repo, file, content, message, branch)
  arg_is_chr(after, allow_null = TRUE)

  purrr::pwalk(
    list(repo, file, content, after, message, branch),
    function(repo, file, content, after, message, branch) {

      cur_content = get_file(repo, file, branch)

      if (is.null(cur_content)) {
        usethis::ui_oops(
          "Unable to retrieve {usethis::ui_value(format_repo(repo, branch, file))}."
        )
      } else {

        if (!is.null(after)) {
          pat_loc = regexec(after, cur_content)
          if (length(pat_loc) == 0) {
            usethis::ui_oops(
              "Unable to find pattern {usethis::ui_value(after)} in {usethis::ui_value(format_repo(repo, branch, file))}."
            )
            return(NULL)
          }

          split_loc =  pat_loc[[1]][[1]] +  attr(pat_loc[[1]], "match.length")

          content = paste0(
            substr(cur_content, 1, split_loc-1),
            content,
            substr(cur_content, split_loc, nchar(cur_content))
          )
        }

        put_file(repo, file, content, message, branch)
        usethis::ui_done("Added content to {usethis::ui_value(format_repo(repo, branch, file))}.")
      }
    }
  )
}

github_api_code_search = function(query) {
  gh::gh("GET /search/code", q = query,
     .token = get_github_token(),
     .limit = get_github_api_limit())
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


file_exists = function(repo, file, branch = "master") {
  purrr::pmap_lgl(
    list(repo, file, branch),
    function(repo, file, branch) {
      if (branch == "master") {
        (length(find_file(repo,file)) > 0)
      } else {
        # Only the master branch is indexed for search
        is.null(get_file(repo, file, branch))
      }
    }
  )
}

github_api_put_file = function(repo, path, content, message, branch) {
  args = list(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = path,
    content = base64enc::base64encode(content),
    message = message, branch = branch,
    .token = get_github_token()
  )

  # To update an existing file we need its current SHA,
  # if the file does not exist this will be NULL.
  cur_file = get_file(repo, path, branch)
  args[["sha"]] = attr(cur_file, "sha")

  do.call(gh::gh, args)
}


#' Low level function for adding a file to a Github repository
#'
#' @param repo Character. Address of repository in `owner/name` format.
#' @param path Character. File path within the repository.
#' @param content Character or raw. Content of the file.
#' @param message Character. Commit message. If not provided, a custom character string will be created.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param verbose Logical. Should success / failure messages be printed
#'
#' @family file functions
#'
#' @export
#'
put_file = function(repo, path, content, message = NULL, branch = "master", verbose = TRUE) {

  arg_is_scalar(content) # Could be character or raw
  arg_is_chr_scalar(repo, path, branch)
  arg_is_chr_scalar(message, allow_null = TRUE)

  if (is.null(message))
    message = glue::glue("Adding file: {path}")

  if (is.character(content))
    content = charToRaw(content)

  res = purrr::safely(github_api_put_file)(repo, path, content, message, branch)

  if(verbose){
    status_msg(
      res,
      glue::glue("Added file {usethis::ui_value(path)} to repo {usethis::ui_value(repo)}."),
      glue::glue("Failed to add file {usethis::ui_value(path)} to repo {usethis::ui_value(repo)}.")
    )
  }

  res
}


github_api_get_commits = function(repo, sha=NULL, path=NULL, author=NULL, since=NULL, until=NULL) {
  args = list(
    endpoint = "GET /repos/:owner/:repo/commits",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = get_github_token()
  )

  args[["sha"]] = sha
  args[["path"]] = path
  args[["author"]] = author
  args[["since"]] = since
  args[["until"]] = until

  do.call(gh::gh, args)
}

get_committer = function(repo, sha=NULL, path=NULL, author=NULL, since=NULL, until=NULL) {

  arg_is_chr(repo)
  arg_is_chr_scalar(repo, sha, path, author, since, until, allow_null=TRUE)

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
          sha  = character(),
          user = character(),
          date = character(),
          msg  = character()
        )
      } else {
        tibble::tibble(
          repo = repo,
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
  commits = get_committer(repo, sha=branch, path=path)
  nrow(commits) > 1
}





#' Add files to a repo
#'
#' `add_file` uses the GitHub API to add/update files in an existing repo on GitHub. Note that due to time delays in caching, files that have been added very recently might not yet be displayed as existing and might accidentally be overwritten.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param message Character. Commit message. If not provided, a custom character string will be created, in the form of "Added file(s): filename(s)". If this custom message character length exceeds 50, it will be shortened to "Added file(s)".
#' @param file Character. Local file path(s) of file or files to be added.
#' @param branch Character. Name of branch to use, defaults to "master".
#' @param preserve_path Logical. Should the local relative path be preserved.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' add_file("rundel/ghclass", "Update DESCRIPTION", "./DESCRIPTION")
#' }
#'
#' @family file functions
#'
#' @export
#'
add_file = function(repo, file, message = NULL, branch = "master",
                    preserve_path = FALSE, overwrite = FALSE) {

  arg_is_chr(repo, file, branch)
  arg_is_chr(message, allow_null = TRUE)
  arg_is_lgl_scalar(preserve_path, overwrite)

  file_status = fs::file_exists(file)
  if (any(!file_status))
    usethis::ui_stop("Unable to locate the following file(s): {usethis::ui_value(file)}")

  if (is.null(message))
    message = list(NULL)

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  purrr::pwalk(
    list(repo, file, message, branch),
    function(repo, file, message, branch){
      purrr::walk(
        file,
        function(file){
          gh_path = file
          if(!preserve_path)
            gh_path = fs::path_file(file)

          if(!check_file_modification(repo, gh_path, branch) | overwrite){
            put_file(
              repo = repo,
              path = gh_path,
              content = paste(readLines(file), collapse = "\n"),
              message = message,
              branch = branch,
              verbose = TRUE
            )
          } else {
            usethis::ui_oops( paste(
              'Failed to add file {usethis::ui_value(gh_path)} to repo {usethis::ui_value(repo)}, file already exists.',
              'If you want to force add this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
            ) )
          }
        })
    })
}
