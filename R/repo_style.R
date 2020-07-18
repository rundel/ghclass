#' Style repository with styler
#'
#' * `repo_style` implements "non-invasive pretty-printing of R source code" of .R or .Rmd files
#' within a repository using the `styler` package and adhering to `tidyverse` formatting guidelines.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param files Character or vector of characters. Names of .R and/or .Rmd files that styler should be applied to.
#' @param branch Character. Name of new branch to be created or overwritten. Default is "styler".
#' @param base Character. Name of branch that contains the .R and/or .Rmd files to be styled
#' @param create_pull_request Logical. If TRUE, a pull request is created from branch to base.
#' @param draft Logical. Should the pull request be created as a draft pull request? (Draft PRs cannot be merged
#'   until allowed by the author)
#' @param tag_collaborators Logical. If TRUE, a message with the repository collaborators is displayed.
#' @param prompt Character. Prompt the user before overwriting an existing branch.
#'
#' @export
#'
repo_style = function(repo, files = c("*.R", "*.Rmd"), branch = "styler", base = "master",
                      create_pull_request = TRUE, draft = TRUE, tag_collaborators = TRUE,
                      prompt = TRUE) {
  require_styler()
  require_gert()

  arg_is_chr(repo, files, branch, base)
  arg_is_lgl(draft)
  arg_is_lgl_scalar(create_pull_request, tag_collaborators)

  dir = file.path(tempdir(),"styler")

  # Make sure the directory is empty
  unlink(dir, recursive = TRUE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  purrr::pwalk(
    list(repo, base, branch, draft),
    function(repo, base, branch, draft) {
      withr::local_dir(dir)
      path = get_repo_name(repo)
      unlink(path, recursive = TRUE)

      return_on_any_failed( local_repo_clone(repo, local_path = dir, branch = base) )
      return_on_any_failed( local_repo_branch(path, branch = branch) )

      #branch_exists = paste0("origin/",branch) %in% gert::git_branch_list(path)[["name"]]

      file_paths = unlist(
        purrr::map(files, ~ fs::dir_ls(path, recurse = TRUE, glob = .x)),
        use.names = FALSE
      )

      if (length(file_paths) == 0) {
        cli::cli_alert_danger("Found 0 files with the glob {.val {files}} in repo {.val {repo}}")
        return()
      }

      output = utils::capture.output( styler::style_file(file_paths) )
      msg = c("Results of running styler:\n", output)
      msg = paste(msg, collapse = "\n")

      # Check if styler didnt fix anything
      status = gert::git_status(path)
      if (nrow(status) == 0) {
        cli::cli_alert_info("No changes were suggested by styler")
        return()
      }

      return_on_any_failed( local_repo_add(path) )
      return_on_any_failed( local_repo_commit(path, message = msg) )
      return_on_any_failed( local_repo_push(path, branch = branch, force = TRUE, prompt = prompt) )

      if (create_pull_request) {
        msg = paste(c(
          "This pull request contains the results of running the automated R code formating tool styler ",
          "on your repo. Styling is based on the [tidyverse R style guide](http://style.tidyverse.org).\n",
          "\n",
          "Click on the commit below to see details of recommended changes. It is not necessary that your ",
          "code cleanly pass these checks, but if there is a large number of significant changes suggested ",
          "you should review the style guide with an eye towards potentially improving your code formatting."
        ), collapse = "")

        #if (tag_collaborators) {
        #  users = repo_collaborators(repo, include_admins = FALSE)[["username"]]
        #  if (length(users) > 0)
        #    msg = paste0(msg,"\n\n", paste0("@", users, collapse = ", "))
        #}

        open_prs = repo_prs(repo, state="open")
        pr_already_open = any(  (base == open_prs[["base_ref"]])
                              & (branch == open_prs[["head_ref"]]) )

        if (pr_already_open) {
          details = cli_glue("{repo} ({base} <- {branch})")

          cli::cli_alert_info(
            "Pull request {details} already exists. New styler changes should be reflected in this PR."
          )
        } else {
          pr_create(
            repo, title = "styler revisions",
            base = base, head = branch,
            body = paste0(msg, collapse = "\n"),
            draft = draft
          )
        }
      }
    }
  )
}
