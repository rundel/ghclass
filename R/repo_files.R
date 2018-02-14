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

find_files = function(repo, file_pattern, file_extension, limit=NULL)
{
  stopifnot(length(repo)==1)

  q = paste0("repo:",repo)

  if (!missing(file_pattern))
    q = paste(q, paste0("filename:", file_pattern, collapse=" "))
  if (!missing(file_extension))
    q = paste(q, paste0("extension:", file_extension, collapse=" "))

  res = gh("GET /search/code",
           q=q, .token=get_github_token(), .limit=limit)

  map_chr(res[["items"]], "path")
}


file_exists = function(repos, files, branch = "master")
{
  purrr::pmap_lgl(
    list(repos, files, branch),
    function(repo, file, branch) {
      if (branch == "master") {
        q = paste0("repo:", repo,
                   " path:", fs::path_dir(file),
                   " filename:", fs::path_file(file))

        res = gh("GET /search/code",
                 q=q, .token=get_github_token(), .limit=NULL)

        (length(res[["items"]]) > 0)
      } else {
        is.null(get_file(repo, file, branch))
      }
    }
  )
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

  stopifnot(all(fs::file_exists(files)))
  stopifnot(all(check_repos(repo)))

  if (is.character(files) & length(repo) != length(files))
    files = list(files)

  purrr::pwalk(
    list(repo, message, files),
    function(repo, message, files) {

      name = get_repo_name(repo)
      owner = get_repo_owner(repo)

      if (verbose)
        message("Adding files to", repo, "...")

      gh_paths = files
      if (!preserve_path)
        gh_paths = fs::path_file(files)

      purrr::walk2(
        files, gh_paths,
        function(file, gh_path) {
          args = list(
            endpoint = "PUT /repos/:owner/:repo/contents/:path",
            owner = owner, repo = name, path = gh_path,
            content = base64enc::base64encode(file),
            message = message, branch = branch,
            .token = get_github_token()
          )

          gh_file = get_file(repo, gh_path, branch)
          if (!is.null(gh_file))
            args[["sha"]] = gh_file$sha

          res = do.call(purrr::safely(gh::gh), args)

          if (any(check_errors(res))) {
            message("Adding ", file, " to ", repo, " failed.")
            if (verbose)
              get_errors(res) %>% format_errors() %>% message()
          }
        }
      )
    }
  )
}
