github_api_repo = function(repo) {
  arg_is_chr_scalar(repo)

  ghclass_api_v3_req(
    endpoint = "GET /repos/:owner/:repo",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    # Needed for template repos
    .send_headers = c(Accept = "application/vnd.github.baptiste-preview+json")
  )
}

#' @rdname repo_core
#' @param strict Logical. Should the old name of a renamed repositories be allowed.
#' @param quiet Logical. Should details on renamed repositories be printed.
#' @export
#'
repo_exists = function(repo, strict = FALSE, quiet = FALSE) {

  arg_is_chr(repo)
  arg_is_lgl_scalar(strict, quiet)

  # Checking if repo exists
  repo_details = purrr::map(repo, purrr::safely(github_api_repo))
  repo_exists = purrr::map_lgl(repo_details, succeeded)

  cur_names = purrr::map_chr(repo_details, c("result","full_name"), .default = NA)
  cur_names = replace_nas(cur_names, repo)

  renamed = cur_names != repo

  if (quiet) {
    purrr::walk2(
      repo[renamed], cur_names[renamed],
      ~ cli::cli_alert_info("Repo {.val {.x}} has been renamed to {.val {.y}}.")
    )
  }

  if (strict)
    repo_exists = repo_exists & !renamed

  repo_exists
}
