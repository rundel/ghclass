#' Create reviewer feedback form
#'
#' `peer_form_create_review` creates blank feedback forms for reviewers based on the user-specified number of questions.
#'
#' @param n Numerical. Number of score fields to be included in the YAML of the .Rmd file.
#' @param title Character. Title of form, defaults to "Reviewer feedback form."
#' @param fname Character. File name of RMarkdown document to be written to memory, defaults to `feedback_blank_review`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to `TRUE`.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param double_blind Logical. If `double_blind = TRUE`, the YAML will contain an `author` field, defaults to `TRUE`.
#'
#' @examples
#' \dontrun{
#' peer_form_create_review(5, "Reviewer feedback for HW2", "rfeedback_hw2_blank")
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_form_create_review = function(n, title = "Reviewer feedback form", fname = "feedback_blank_review",
                                   output = "github_document", write_rmd = TRUE, overwrite = FALSE,
                                   double_blind = TRUE) {
  stopifnot(!is.null(fname))
  if (grepl("\\s+", fname)) {
    fname = gsub("\\s", "_", fname)
  }
  if (grepl("\\.Rmd$", fname)) {
    fname = gsub("\\.Rmd$", "", fname)
  }

  # YAML
  yaml_txt = sprintf(if (!double_blind) {
    "---\ntitle: \"%s\"\nauthor: [INSERT NAME]\noutput: %s\nparams:\n%s\n---\n\n\n"
  } else {
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n"
  },
  title,
  output,
  paste(purrr::map_chr(1:n, function(x) {
    paste0("  q", x, "_score: [INSERT SCORE]")
  }),
  collapse = "\n"))


  # Body
  resp = "Your response goes here..."
  body_txt = paste(
    "## Instructions",
    "Enter your feedback for each question below. Please replace `[INSERT SCORE]` in the `q*_score` fields in the YAML with the scores you give the author for each question.",
    "Please keep your review comments constructive and professional.",
    "## Feedback",
    paste0(purrr::map(
      1:n,
      ~ paste0(
        sprintf(
          "#### %1$i. Place Question %1$i text here. [max. xxx points]\n\n",
          .x
        ),
        resp,
        collapse = "\n\n"
      )
    ),
    collapse = "\n\n"),
    sep = "\n\n"
  )

  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")

  if (write_rmd) {
    fname = paste0(fname, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, file = fname)
      cli::cli_alert_success("Saved file {.file {fname}}")
    } else {
      cli::cli_alert_danger( paste(
        'File {.val {fname}} already exists.',
        'If you want to force save this file, re-run the command with {.code overwrite = TRUE}.'
      ) )
    }
  } else {
    doc_txt
  }
}
