get_readme = function(repo, branch="master")
{
  stopifnot(length(repo) == 1)
  stopifnot(length(file) == 1)

  repo_name  = get_repo_name(repo)
  repo_owner = get_repo_owner(repo)

  purrr::possibly(gh::gh, NULL)(
    "GET /repos/:owner/:repo/readme",
    owner = repo_owner, repo = repo_name, ref = branch,
    .token=get_github_token(), .limit=get_github_api_limit()
  )
}

get_file = function(repo, file, branch="master")
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
add_content = function(repo, file, content, after=NULL, message="Added content", branch="master", verbose=TRUE) {
  require_valid_repo(repo)

  purrr::pwalk(
    list(repo, file, content, after, message, branch),
    function(repo, file, content, after, message, branch) {

      gh_file = get_file(repo, file, branch)

      if (!is.null(gh_file)) {

        cur_content = rawToChar(base64enc::base64decode(gh_file$content))
        if (!is.null(after)) {
          pat_loc = regexec(after, cur_content)
          if (length(pat_loc) == 0) {
            warning("Unable to match pattern: \"", after,"\" in ", repo,"/",file,".", sep="")
            content = paste0(content, cur_content)
          }
          split_loc =  pat_loc[[1]][[1]] +  attr(pat_loc[[1]], "match.length")


          content = paste0(
            substr(cur_content, 1, split_loc-1),
            content,
            substr(cur_content, split_loc, nchar(cur_content))
          )
        }
      }

      if (verbose)
        message("Adding content to ", org, "/", repo, "/", file, " ...")

      put_file(repo, file, charToRaw(content), message, branch)
    }
  )
}

#' @export
replace_content = function(repo, file, pattern, replacement, message="Replaced content", branch="master") {
  require_valid_repo(repo)

  purrr::pwalk(
    list(repo, file, pattern, replacement, message, branch),
    function(repo, file, pattern, replacement, message, branch) {

      gh_file = get_file(repo, file, branch)

      if (is.null(gh_file)) {
        warning("Unable to locate ", repo, "/", file, ".", sep="")
      }

      cur_content = rawToChar(base64enc::base64decode(gh_file$content))

      content = gsub(pattern, replacement, cur_content)
      put_file(repo, file, charToRaw(content), message, branch)

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
        is.null(get_file(repo, file, branch))
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

  args = list(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = file,
    content = base64enc::base64encode(content),
    message = message, branch = branch,
    .token = get_github_token()
  )

  gh_file = get_file(repo, file, branch)
  if (!is.null(gh_file))
    args[["sha"]] = purrr::pluck(gh_file, "sha")

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
