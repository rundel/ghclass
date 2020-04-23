github_api_repo_mirror_template = function(template_repo, repo, private){
  gh::gh(
    "POST /repos/:template_owner/:template_repo/generate",
    template_owner = get_repo_owner(template_repo),
    template_repo = get_repo_name(template_repo),
    owner = get_repo_owner(repo),
    name = get_repo_name(repo),
    private = private,
    .token = github_get_token(),
    .send_headers = c(Accept = "application/vnd.github.baptiste-preview+json")
  )
}


#' Mirror template repository
#'
#' `repo_mirror_template` mirrors the content of a source (template) repository to a new (not yet created)
#' repository, or vector of repositories.
#'
#' @param template_repo Character. Address of template repository in `owner/name` format.
#' @param repo Character. One or more repository addresses in `owner/name` format.
#' Note when using template repos these new repositories must not already exist.
#' @param private Logical. Should the new repository be private or public.
#'
#' @export
#'
repo_mirror_template = function(template_repo, repo, private = TRUE) {
  arg_is_chr_scalar(template_repo)
  arg_is_chr(repo)
  arg_is_lgl_scalar(private)

  repo = unique(repo)
  exists = repo_exists(repo)

  res = purrr::map2(
    repo, exists,
    function(repo, exists) {
      res = purrr::safely(
        function() {
          if (exists) {
            usethis::ui_stop( paste(
              "Cannot mirror (template) to repo {usethis::ui_value(repo)} because",
              "this reposistory already exists."
            ) )
          }

          github_api_repo_mirror_template(template_repo, repo, private)
        }
      )()

      status_msg(
        res,
        glue::glue("Mirrored {usethis::ui_value(template_repo)} to {usethis::ui_value(repo)}."),
        glue::glue("Failed to mirror {usethis::ui_value(template_repo)} to {usethis::ui_value(repo)}.")
      )

      res
    }
  )

  invisible(res)
}
