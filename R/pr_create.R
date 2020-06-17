github_api_pr_create = function(repo, head, base, title, body, draft = TRUE){
  gh::gh(
    "POST /repos/:owner/:repo/pulls",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    head = head,
    base = base,
    title = title,
    body = body,
    draft = draft,
    .token = github_get_token(),
    .send_headers = c(Accept = "application/vnd.github.shadow-cat-preview+json")
  )
}

#' Create pull request
#'
#' `pr_create` creates a pull request on GitHub from the `base` branch to the `head` branch.
#'
#' @param repo Character. Address of one or more repositories in "owner/name" format.
#' @param title Character. Title of the pull request.
#' @param head Character. The name of the branch where your changes are implemented.
#' For cross-repository pull requests in the same network, namespace `head` with a user
#' like this: `username:branch`.
#' @param base Character. The name of the branch you want the changes pulled into.
#' This should be an existing branch on the current repository. You cannot submit
#' a pull request to one repository that requests a merge to a base of another repository.
#' @param body Character. The text contents of the pull request.
#' @param draft Logical. Should the pull request be created as a draft pull request
#' (these cannot be merged until allowed by the author).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' repo_create("ghclass-demo", "test_pr", auto_init=TRUE)
#'
#' branch_create("ghclass-demo/test_pr", new_branch = "test")
#'
#' repo_modify_file("ghclass-demo/test_pr", "README.md", pattern = "test_pr",
#'                  content = "Hello", method = "after", branch = "test")
#'
#' pr_create("ghclass-demo/test_pr", "merge", "test")
#'
#' repo_delete("ghclass-demo/test_pr", prompt = FALSE)
#' }
pr_create = function(repo, title, head, base = "master", body = "", draft = FALSE) {

  arg_is_chr(repo, title, base, head, body)
  arg_is_lgl(draft)

  purrr::pwalk(
    list(repo, base, head, title, body, draft),
    function(repo, base, head, title, body, draft) {
      res = purrr::safely(github_api_pr_create)(
        repo, base = base, head = head, title = title, body = body, draft = draft
      )

      details = cli_glue("{repo} ({base} <- {head})")

      status_msg(
        res,
        "Created pull request for {.val {details}}.",
        "Failed create pull request for {.val {details}}."
      )
    }
  )
}
