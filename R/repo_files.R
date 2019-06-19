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

#' @export
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

#' @export
get_file = function(repo, file, branch = "master") {
  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)
  stopifnot(length(branch) == 1)

  file = purrr::possibly(github_api_get_file, NULL)(repo, file, branch)

  extract_content(file)
}

#' @export
add_content = function(repo, file, content, after = NULL, message = "Added content", branch = "master") {
  #TO DO: Fix since require_valid_repo is no longer vectorized
  #require_valid_repo(repo)

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

# Note: This function is currently not vectorized
find_file = function(repo, file){

  stopifnot(length(repo) == 1)
  #TO DO: Fix since require_valid_repo is no longer vectorized
  #require_valid_repo(repo)

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
        } else {
          usethis::ui_oops("Cannot find file {usethis::ui_value(file)} on {usethis::ui_value(repo)}.")
        }

      }
    )
  )
}


file_exists = function(repo, file, branch = "master")
{
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
  args[["sha"]] = attr(get_file(repo, path, branch), "sha")

  do.call(gh, args)
}

#' @export
put_file = function(repo, path, content, message, branch = "master", verbose = F) {
  stopifnot(length(repo) == 1)
  stopifnot(length(path) == 1)
  stopifnot(length(content) == 1)
  stopifnot(length(message) == 1)
  stopifnot(length(branch) == 1)

  if (is.character(content))
    content = charToRaw(content)

  res = purrr::safely(github_api_put_file)(repo, path, content, message, branch)

  if(verbose){
    status_msg(
      res,
      glue::glue("Added {usethis::ui_value(path)} to {usethis::ui_value(repo)}."),
      glue::glue("Failed to add {usethis::ui_value(path)} to {usethis::ui_value(repo)}.")
    )
  }

  res

}


#' Create file URL to pass to GitHub Commit API
#'
#' `create_file_commit_url` creates a file URL for a single file that can be passed to a `gh::gh(METHOD URL)` query. The query is limited to 100 entries.
#'
#'
create_file_commit_url = function(repo, gh_path){

  stopifnot(length(repo) == 1)
  stopifnot(length(gh_path) == 1)

  owner = get_repo_owner(repo)
  name = get_repo_name(repo)
  paste0("https://api.github.com/repos/",
         owner, "/",
         name, "/commits?path=",
         gh_path,
         "&page=1&per_page=100")
}

github_api_get_commit_history = function(repo, gh_path){

  safe_gh(paste0("GET ", create_file_commit_url(repo, gh_path)),
          .token=get_github_token())

}

#' Check for modifications of file on GitHub
#'
#' `check_file_modification` checks whether a file on GitHub has previously been modified by a commit (not taking into account the initial commit).
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param file Character. Name of file.
#' @param include_admin Logical. Should users with admin privileges be included in checking for previous file modifications.
#'
#' @return TRUE or FALSE
#'
check_file_modification = function(repo, gh_path, include_admin){

  res = github_api_get_commit_history(repo, gh_path)

  if(length(res$result) > 1){

    user = unique(purrr::map_chr(res$result, c("author", "login")))

    if(!include_admin){
      user = setdiff(user, unlist(get_admin(get_repo_owner(repo))))
    }

    if(!is.null(user)){
      purrr::walk(user,
                  function(user){
                    usethis::ui_oops("Adding {usethis::ui_value(gh_path)} to {usethis::ui_value(repo)} overwrites modifications by student(s): {usethis::ui_value(user)}.")
                    usethis::ui_info("If you want to overwrite modifications, re-run with overwrite = TRUE.")
                    message(" ")
                  })
    }
  }
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
#' @param include_admin Logical. Should users with admin privileges be included in checking for previous file modifications, defaults to FALSE.
#'
#' @examples
#' \dontrun{
#' add_file("rundel/ghclass", "Update DESCRIPTION", "./DESCRIPTION")
#' }
#'
#' @aliases grab_repos
#'
#' @family local repo functions
#'
#' @export
#'
add_file = function(repo, file, message = NULL, branch = "master", preserve_path = FALSE, overwrite = FALSE, include_admin = FALSE){

  stopifnot(!missing(repo))
  stopifnot(!missing(file))

  # Format commit message
  if (is.null(message)) {
    if (length(file) == 1) {
      message <- glue::glue("Added file: {file}")
    }
    if (length(file) == 2) {
      file_collapsed <- glue::glue_collapse(file, sep = " and ")
      message <- glue::glue("Added files: {file_collapsed}")
    }
    if (length(file) > 2) {
      file_collapsed <- glue::glue_collapse(file, sep = ", ", last = ", and ")
      message <- glue::glue("Added files: {file_collapsed}")
    }
    if (nchar(message) > 50) {
      message <- "Added file(s)"
    }
  }

  file_status = fs::file_exists(file)
  if (any(!file_status))
    stop("Unable to locate the following file(s):\n", format_list(file[!file_status]),
         call. = FALSE)

  if (is.character(file) & (length(file) > 1))
    file = list(file)

  purrr::pwalk(list(repo, file, message, branch, include_admin),

              function(repo, file, message, branch, include_admin){

                purrr::walk(file,
                            function(file){
                              gh_path = file
                              if(!preserve_path)
                                gh_path = fs::path_file(file)

                              if(!file_exists(repo, gh_path, branch) | overwrite){

                                content = paste(readLines(file), collapse = "\n")
                                res = put_file(repo = repo,
                                               path = gh_path,
                                               content = content,
                                               message = message,
                                               branch = branch,
                                               verbose = T)

                              } else {

                                usethis::ui_oops("Failed to add {usethis::ui_value(gh_path)} to {usethis::ui_value(repo)}: already exists.")

                                modified = check_file_modification(repo, gh_path, include_admin)
                                if(length(modified) == 0){
                                  usethis::ui_info("If you want to commit {usethis::ui_value(gh_path)} to {usethis::ui_value(repo)} again, re-run with overwrite = TRUE.")
                                  message(" ")
                                }
                              }
                            })

               })
}


################# Deprecated functions ###################

#' Add files to a repo
#'
#' \code{add_files} uses the GitHub api to add/update files in an existing repo on GitHub.
#'
#' @param repo repo names in the form of \emph{owner/name}.
#' @param message commit message.
#' @param files local file paths of files to be added.
#' @param branch name of branch to use, defaults to master.
#' @param preserve_path should the local relative path be preserved.
#'
#' @templateVar fun add_files
#' @template template-depr_fun
#'
#' @examples
#' \dontrun{
#' add_files("rundel/ghclass", "Update DESCRIPTION", "./DESCRIPTION")
#' }
#'
#' @templateVar old add_files
#' @templateVar new add_file
#' @template template-depr_pkg
#'
#' @aliases grab_repos
#'
#' @family local repo functions
#'
#' @export
#'
add_files = function(repo, message, files, branch = "master", preserve_path = FALSE)
{
  .Deprecated(msg = "'add_files' is deprecated and will be removed in the next version. Use 'add_file' instead.",
              new = "add_file")

  stopifnot(!missing(repo))
  stopifnot(!missing(message))
  stopifnot(!missing(files))

  file_status = fs::file_exists(files)
  if (any(!file_status))
    stop("Unable to locate the following files:\n", format_list(files[!file_status]),
         call. = FALSE)

  if (is.character(files) & length(repo) != length(files))
    files = list(files)

  purrr::pwalk(
    list(repo, message, files),
    function(repo, message, files) {

      gh_paths = files
      if (!preserve_path)
        gh_paths = fs::path_file(files)

      purrr::walk2(
        gh_paths, files,
        function(path, file, repo, message, branch) {
          content = paste(readLines(file), collapse = "\n")
          put_file(repo, path, content, message, branch)
        },
        repo = repo, message = message, branch = branch
      )
    }
  )
}
