#' @export
get_readme = function(repo, branch="master")
{
  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)

  repo_name  = get_repo_name(repo)
  repo_owner = get_repo_owner(repo)

  readme = purrr::possibly(gh::gh, NULL)(
    "GET /repos/:owner/:repo/readme",
    owner = repo_owner, repo = repo_name, ref = branch,
    .token=get_github_token(), .limit=get_github_api_limit()
  )

  if (!is.null(readme)) {
    readme = rawToChar(base64enc::base64decode(readme$content))
  }

  readme
}

get_file_handle = function(repo, file, branch)
{
  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)

  repo_name  = get_repo_name(repo)
  repo_owner = get_repo_owner(repo)

  purrr::possibly(gh::gh, NULL)(
    "GET /repos/:owner/:repo/contents/:path",
    owner = repo_owner, repo = repo_name, path=file,
    ref = branch,
    .token=get_github_token(), .limit=get_github_api_limit()
  )
}

#' @export
get_file = function(repo, file, branch="master")
{
  file = get_file_handle(repo, file, branch)
  if (!is.null(file))
    file = rawToChar(base64enc::base64decode(file$content))

  file
}

#' @export
get_file_sha = function(repo, file, branch="master")
{
  get_file_handle(repo, file, branch)[["sha"]]
}

#' @export
add_content = function(repo, file, content, after=NULL, message="Added content", branch="master", verbose=TRUE) {
  require_valid_repo(repo)

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



find_file = function(repo, file)
{
  stopifnot(length(repo)==1)
  require_valid_repo(repo)

  purrr::flatten_chr(
    purrr::map(
      file,
      function(file) {
        q = paste0("repo:", repo,
               " path:", fs::path_dir(file),
               " filename:", fs::path_file(file))

        res = gh("GET /search/code", q=q,
                 .token=get_github_token(), .limit=get_github_api_limit())

        purrr::map_chr(res[["items"]], "path")
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
        is.null(get_file_handle(repo, file, branch))
      }
    }
  )
}

put_file = function(repo, file, content, message, branch)
{
  stopifnot(length(repo)==1)
  stopifnot(length(file)==1)
  stopifnot(length(message)==1)
  stopifnot(length(branch)==1)

  if (is.character(content))
    content = charToRaw(content)

  args = list(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = file,
    content = base64enc::base64encode(content),
    message = message, branch = branch,
    .token = get_github_token()
  )
  args[["sha"]] = get_file_sha(repo, file, branch)

  do.call(safe_gh, args)
}



#' Add files to a repo
#'
#' \code{add_files} uses the GitHub api to add/update files in an existing repo on GitHub.
#'
#' @param repo repo names in the form of \emph{owner/name}.
#' @param message commit message.
#' @param files local file paths of files to be added.
#' @param branch name of branch to use, defaults to master.
#' @param preserve_path should the local relative path be preserved.
#' @param verbose display verbose output.
#'
#' @examples
#' \dontrun{
#' add_files("rundel/ghclass", "Update DESCRIPTION", "./DESCRIPTION")
#' }
#'
#' @aliases grab_repos
#'
#' @family local repo functions
#'
#' @export
#'
add_files = function(repo, message, files, branch = "master", preserve_path=FALSE, verbose=TRUE)
{
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

      name = get_repo_name(repo)
      owner = get_repo_owner(repo)

      if (verbose)
        message("Adding files to ", repo, " ...")

      gh_paths = files
      if (!preserve_path)
        gh_paths = fs::path_file(files)

      res = purrr::map2(
        gh_paths, files,
        put_file,
        repo = repo, message = message, branch = branch
      )

      check_result(
        res, sprintf("Failed to add files to %s.", repo),
        verbose, error_prefix = paste0(files,": ")
      )
    }
  )
}
