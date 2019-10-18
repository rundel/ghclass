github_api_org_repo_create = function(repo, private, auto_init, gitignore_template){
  gh::gh("POST /orgs/:owner/repos",
         owner = get_repo_owner(repo),
         name = get_repo_name(repo),
         private = private,
         auto_init = auto_init,
         gitignore_template = gitignore_template,
         .token = github_get_token())
}

#' Create repository
#'
#' `repo_create` creates either individual or team repositories.
#'
#' @param org Character. Name of the GitHub organization.
#' @param name Character. One or more GitHub username or team name.
#' @param prefix Character. Common repository name prefix
#' @param suffix Character. Common repository name suffix
#' @param private Logical. Create private repositories?
#' @param auto_init Logical. Initialize the repository with a README.md?
#' @param gitignore_template Character. .gitignore template language.
#'
#' @return A character vector of created repository addresses.
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-test", c("user01","user02"), prefix = "hw01-")
#' }
#'

#' @aliases create_repo
#'
#' @export
#'
repo_create = function(org, name,
                       prefix = "", suffix = "",
                       private = TRUE, auto_init = FALSE,
                       gitignore_template = "R") {

  arg_is_chr(name)
  arg_is_chr_scalar(org, prefix, suffix, gitignore_template)
  arg_is_lgl_scalar(private, auto_init)

  name = unique(name)

  repo = paste0(prefix, name, suffix)
  repo = paste0(org, "/", repo)

  if (length(repo) != length(unique(repo)))
    usethis::ui_stop("Not all repo names are unique: {usethis::ui_value(repo)}.")

  exists = repo_exists(repo)

  res = purrr::map2(
    repo, exists,
    function(repo, exists) {
      if (exists) {
        usethis::ui_info("Skipping repo {usethis::ui_value(repo)}, it already exists.")
        return()
      }
      res = purrr::safely(github_api_org_repo_create)(
        repo,
        private = private,
        auto_init = auto_init,
        gitignore_template = gitignore_template
      )

      status_msg(
        res,
        glue::glue("Created repo {usethis::ui_value(repo)}."),
        glue::glue("Failed to create repo {usethis::ui_value(repo)}.")
      )

      ternary(succeeded(res), repo, NULL)
    }
  )

  invisible(purrr::flatten_chr(res))
}
