#' Create author rating form
#'
#' `peer_form_create_rating` creates a short feedback form for authors to rate the feedback they got from reviewers. The rating categories are based on Reily, K. and P. Ludford Finnerty,  and L. Terveen (2009): Two Peers Are Better Than One: Aggregating Peer Reviews for Computing Assignments is Surprisingly Accurate. In *Proceedings of the ACM 2009 International Conference on Supporting Group Work*. GROUP’09, May 10–13, 2009, Sanibel Island, Florida, USA.
#'
#' @param category Character. Categories to be included in the feedback form, defaults to `c("helpfulness", "accuracy", "fairness")`.
#' @param title Character. Title of form, defaults to "Author rating form".
#' @param fname Character. File name of RMarkdown document to be written to memory, defaults to `feedback_blank_rating`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to `FALSE`.
#' @param allow_comment Logical. Should optional comment field be included? Defaults to `FALSE`.
#'
#' @examples
#' \dontrun{
#' peer_form_create_rating(c("accuracy", "fairness"))
#' }
#'
#' @family peer review functions
#'
#' @export
#'
peer_form_create_rating = function(category = c("helpfulness", "accuracy", "fairness"),
                                   title = "Author rating form",
                                   fname = "feedback_blank_rating",
                                   output = "github_document",
                                   write_rmd = TRUE,
                                   overwrite = FALSE,
                                   allow_comment = FALSE) {
  arg_is_chr(category)
  stopifnot(all(category %in% c("helpfulness", "accuracy", "fairness")))

  stopifnot(!is.null(fname))
  if (grepl("\\s+", fname)) {
    fname = gsub("\\s", "_", fname)
  }
  if (grepl("\\.Rmd$", fname)) {
    fname = gsub("\\.Rmd$", "", fname)
  }

  # YAML
  yaml_txt = sprintf(
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n",
    title,
    output,
    paste(paste0("  ", category, ": [INSERT SCORE]"), collapse = "\n")
  )

  # Instructions
  instruct_txt = sprintf(
    "Please rate the reviewer's feedback based on the categories below on a scale from \"Strongly disagree\" to \"Strongly agree.\" Please replace `[INSERT SCORE]` in the %s %s in the YAML with the scores you give the reviewer for each category.",
    if (length(category) == 1) {
      paste0("`", category, "`")
    } else if (length(category) == 2) {
      paste0(paste0("`", category[1], "`"),
             " and ",
             paste0("`", category[2], "`"))
    } else {
      paste0(paste0(purrr::map_chr(category[1:(length(category) - 1)], ~ paste0("`", .x, "`"))
                    , collapse = ", "),
             ", and ",
             paste0("`", category[length(category)], "`"))
    },
    if (length(category) == 1) {
      "field"
    } else {
      "fields"
    }
  )

  category_txt = list(helpfulness = "`helpfulness`: \"The reviewer's feedback was constructive and helpful.\"  [max. 4 points]",
                      accuracy = "`accuracy`: \"The reviewer's assessment accurately describes the quality of my work.\" [max. 4 points]",
                      fairness = "`fairness`: \"The reviewer's assessment was fair.\" [max. 4 points]")

  tab_txt = paste(
    "| Score | Rating            |",
    "|-------|-------------------|",
    "| 1     | Strongly disagree |",
    "| 2     | Disagree          |",
    "| 3     | Agree             |",
    "| 4     | Strongly agree    |",
    sep = "\n"
  )

  if (allow_comment) {
    comment_txt = "\n---\nIf you have additional comments for the reviewer, please enter it below. Please keep your review comments constructive and professional.\n\n[Your comments go here]"
  } else {
    comment_txt = NULL
  }

  # Putting it all together
  body_txt = paste(
    "## Instructions",
    instruct_txt,
    paste(paste0(
      1:length(category),
      ". ",
      purrr::map_chr(category, ~ paste(category_txt[which(names(category_txt) == .x)]))
    ),
    collapse = "\n\n"),
    tab_txt,
    comment_txt,
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
