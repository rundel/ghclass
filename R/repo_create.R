github_api_org_repo_create = function(repo, private, auto_init, gitignore_template){
  ghclass_api_v3_req(
    endpoint = "POST /orgs/:owner/repos",
    owner = get_repo_owner(repo),
    name = get_repo_name(repo),
    private = private,
    auto_init = auto_init,
    gitignore_template = gitignore_template
  )
}

#' @rdname repo_core
#'
#' @param org Character. Name of the GitHub organization.
#' @param name Character. One or more GitHub username or team name.
#' @param prefix Character. Common repository name prefix
#' @param suffix Character. Common repository name suffix
#' @param private Logical. Create private repositories?
#' @param auto_init Logical. Should the repository be initialized with a `README.md`.
#' @param gitignore_template Character. `.gitignore` language template to use.
#'
#' @export
#'
repo_create = function(
  org, name,
  prefix = "", suffix = "",
  private = TRUE, auto_init = FALSE,
  gitignore_template = "R"
) {

  arg_is_chr(name)
  arg_is_chr_scalar(org, prefix, suffix, gitignore_template)
  arg_is_lgl_scalar(private, auto_init)

  name = unique(name)

  repo = paste0(prefix, name, suffix)
  repo = paste0(org, "/", repo)

  if (length(repo) != length(unique(repo)))
    cli_stop("Not all repo names are unique: {.val {repo}}.")

  exists = repo_exists(repo)

  res = purrr::map2(
    repo, exists,
    function(repo, exists) {
      if (exists) {
        cli::cli_alert_info("Skipping repo {.val {repo}}, it already exists.")
        return(repo)
      }
      res = purrr::safely(github_api_org_repo_create)(
        repo,
        private = private,
        auto_init = auto_init,
        gitignore_template = gitignore_template
      )

      status_msg(
        res,
        "Created repo {.val {repo}}.",
        "Failed to create repo {.val {repo}}."
      )

      ternary(succeeded(res), repo, NULL)
    }
  )

  invisible(purrr::flatten_chr(res))
}
