github_api_repo = function(repo) {
  arg_is_chr_scalar(repo)

  gh::gh(
    "GET /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token(),
    # Needed for template repos
    .send_headers = c(Accept = "application/vnd.github.baptiste-preview+json")
  )
}

#' Check existence of GitHub repository
#'
#' `repo_exists` returns TRUE if the github repository exists.
#' The function also prints a message if a repository has been renamed.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param strict Logical. Specifies whether renamed repositories are allowed.
#' @param verbose Logical. Specifies if details on renamed repositories should be printed.
#'
#' @examples
#' \dontrun{
#' repo_exists(c("rundel/ghclass", "rundel/ghclass_fake"))
#' }
#'
#' @return A logical vector
#'
#' @export
#'
repo_exists = function(repo, strict = FALSE, verbose = TRUE) {

  arg_is_chr(repo)
  arg_is_lgl_scalar(strict, verbose)

  # Checking if repo exists
  repo_details = purrr::map(repo, purrr::safely(github_api_repo))
  repo_exists = purrr::map_lgl(repo_details, succeeded)

  cur_names = purrr::map_chr(repo_details, c("result","full_name"), .default = NA)
  cur_names = replace_nas(cur_names, repo)

  renamed = cur_names != repo

  if (verbose) {
    purrr::walk2(
      repo[renamed], cur_names[renamed],
      ~ cli::cli_alert_info("Repo {.val {.x}} has been renamed to {.val {.y}}.")
    )
  }

  if (strict)
    repo_exists = repo_exists & !renamed

  repo_exists
}
