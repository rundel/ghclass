github_api_repo_mirror_template = function(source_repo, repo, private){
  ghclass_api_v3_req(
    endpoint = "POST /repos/:template_owner/:template_repo/generate",
    template_owner = get_repo_owner(source_repo),
    template_repo = get_repo_name(source_repo),
    owner = get_repo_owner(repo),
    name = get_repo_name(repo),
    private = private,
    .send_headers = c(Accept = "application/vnd.github.baptiste-preview+json")
  )
}

#' @rdname repo_core
#'
#' @param source_repo Character. Address of template repository in `owner/name` format.
#' @param target_repo Character. One or more repository addresses in `owner/name` format.
#' Note when using template repos these new repositories must *not* exist.
#' @param private Logical. Should the new repository be private or public.
#'
#' @export
#'
repo_mirror_template = function(source_repo, target_repo, private = TRUE) {
  arg_is_chr_scalar(source_repo)
  arg_is_chr(target_repo)
  arg_is_lgl_scalar(private)

  target_repo = unique(target_repo)
  exists = repo_exists(target_repo)

  res = purrr::map2(
    target_repo, exists,
    function(repo, exists) {
      res = purrr::safely(
        function() {
          if (exists) {
            cli_stop("Cannot mirror (template) to repo {.val {repo}} because this reposistory already exists.")
          }

          github_api_repo_mirror_template(source_repo, repo, private)
        }
      )()

      status_msg(
        res,
        "Mirrored repo {.val {source_repo}} to repo {.val {repo}}.",
        "Failed to mirror repo {.val {source_repo}} to repo {.val {repo}}."
      )

      res
    }
  )

  invisible(res)
}
