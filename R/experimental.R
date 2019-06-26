#' Style repository
#'
#' `style_repo` implements "non-invasive pretty-printing of R source code" of .R or .Rmd files within a repository using the `styler` package and adhering to `tidyverse` formatting guidelines.
#'
#' @param repo Character. Address of repository in "owner/name" format.
#' @param files Character or vector of characters. Names of .R and/or .Rmd files that styler should be applied to.
#' @param branch Character. Name of new branch to be created. Default is "styler".
#' @param base Character. Name of branch that contains the .R and/or .Rmd files to be reformatted.
#' @param create_pull_request Logical. If TRUE, a pull request is created from branch to base.
#' @param tag_collaborators Logical. If TRUE, a message with the repository collaborators is displayed.
#' @param git Chacacter. Path to the git binary.
#' @param verbose Logical. Display verbose output.
#'
#' @examples
#' \dontrun{
#' style_repo("Sta523-Fa17/base_hw1", files = c("hw1_sample.Rmd"))
#' }
#'
#' @export
#'
style_repo = function(repo, files = c("*.R","*.Rmd"), branch = "styler", base = "master",
                      create_pull_request = TRUE, tag_collaborators = TRUE,
                      git = require_git(), verbose = TRUE) {
  stopifnot(styler_available())
  stopifnot(length(repo) >= 1)

  dir = file.path(tempdir(),"styler")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  on.exit({
    unlink(file.path(dir), recursive = TRUE)
  })

  purrr::pwalk(
    list(repo, base, branch),
    function(repo, base, branch) {
      ## TODO add base to branch
      create_branch(repo, cur_branch = base, new_branch = branch)
      clone_repo(repo, local_path = dir, branch = branch)

      path = fs::path(dir, get_repo_name(repo))

      withr::local_dir(path)

      file_paths = unlist(purrr::map(files, ~ fs::dir_ls(path, recurse = TRUE, glob = .x)),
                          use.names = FALSE)




      msg = c("Results of running styler:\n", utils::capture.output( styler::style_file(file_paths) ))
      writeLines(msg, "commit_msg")

      system(paste0(git, " add ", paste0(file_paths, collapse = " ")),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " commit -F commit_msg"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      system(paste0(git, " push"),
             intern = FALSE, wait = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

      if (create_pull_request) {

        msg = paste(c(
          "This pull request contains the results of running the automated R code formating tool styler ",
          "on your repo. Styling is based on the tidyverse [R style guide](http://style.tidyverse.org)\n",
          "\n",
          "Click on the commit below to see details of recommended changes. It is not necessary that your ",
          "code cleanly pass these checks, but if there is a large number of significant changes suggested ",
          "you should review the style guide with an eye towards potentially improving your code formatting."
        ), collapse = "")

        if (tag_collaborators) {
          users = get_collaborator(repo)[[1]]
          msg = paste0(msg,"\n\n", paste0("@", users, collapse = ", "))
        }

        create_pull_request(
          repo, title = "styler revisions",
          base = base, head = branch,
          body = paste0(msg, collapse = "\n"),
          verbose = verbose
        )
      }
    }
  )
}
