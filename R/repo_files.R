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

put_file = function(repo, gh_path, file, message, branch)
{
  stopifnot(length(repo)==1)
  stopifnot(length(file)==1)
  stopifnot(length(gh_path)==1)
  stopifnot(length(message)==1)
  stopifnot(length(branch)==1)

  args = list(
    endpoint = "PUT /repos/:owner/:repo/contents/:path",
    owner = get_repo_owner(repo), repo = get_repo_name(repo),
    path = gh_path,
    content = base64enc::base64encode(file),
    message = message, branch = branch,
    .token = get_github_token()
  )

  gh_file = get_file(repo, gh_path, branch)
  if (!is.null(gh_file))
    args[["sha"]] = purrr::pluck(gh_file, "sha")

  res = do.call(purrr::safely(gh::gh), args)

  if (any(check_errors(res))) {
    msg = sprintf("Adding %s to %s failed.\n", file, repo)
    if (verbose)
      msg = paste0(msg, format_list(get_errors(res)))

    warning(msg, call. = FALSE, immediate. = TRUE)
  }
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
        message("Adding files to ", repo, "...")

      gh_paths = files
      if (!preserve_path)
        gh_paths = fs::path_file(files)

      purrr::walk2(
        files, gh_paths,
        put_file,
        repo = repo, message = message, branch = branch
      )
    }
  )
}
