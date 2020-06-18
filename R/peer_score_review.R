#' Collect scores from review forms
#'
#' The `peer_score_review()` function collects score information from the YAML of a review form within reviewers' review repositories. It outputs a new .csv file, with rows specifying individual question scores for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param form_review Character. File name of reviewer feedback form (must be .Rmd document).
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' peer_score_review(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' form_review = "hw2_review.Rmd",
#' prefix = prefix)
#' }
#'
#' @importFrom rlang .data
#' @family peer review functions
#'
#' @export
#'
peer_score_review = function(org, roster, form_review, prefix = "", suffix = "", write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, form_review)
  arg_is_lgl(write_csv)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", form_review)) {
    cli_stop("{.field form_review} must be an {.file .Rmd} file.")
  }

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  out_temp = purrr::map_dfr(
    seq_len(nrow(rdf)),
    function(x) {
      repo = as.character(rdf[x, 'repo_rev_review'])
      ghpath = glue::glue("{as.character(rdf[x, 'aut_random'])}/{form_review}")
      rev_no = as.character(rdf[x, 'rev_no'])

      feedback = purrr::safely(repo_get_file)(repo = repo, path = ghpath)

      status_msg(
        feedback,
        "Located file {.val {ghpath}} on repo {.val {repo}}.",
        "Cannot locate file {.val {ghpath}} on repo {.val {repo}}."
      )

      if (succeeded(feedback)) {
        tc = textConnection(feedback[['result']])
        scores = rmarkdown::yaml_front_matter(tc)[['params']]
        scores[scores == "[INSERT SCORE]"] = NA
        scores = purrr::map_chr(scores, ~ gsub("[][]", "", .x))

        inp = stats::setNames(c(as.character(rdf[x, 'aut']), rev_no, scores),
                              c("user", "rev_no", paste0("q", 1:length(scores))))
        tibble::as_tibble(as.list(inp))

      }
    }
  )

  out_temp = tidyr::gather(
    out_temp,
    "q_name",
    "q_value",
    -.data$user,
    -.data$rev_no
  )
  out_temp = tidyr::unite(out_temp, "new", c("rev_no", "q_name"))
  out_temp = tidyr::spread(out_temp, .data$new, .data$q_value)
  out = merge(out_temp, roster, all.y = T)

  out = out[, union(names(roster), names(out))]
  out = out[order(out[['user_random']]),]

  if (write_csv) {
    prefix_for_fname = sub("-$", "", prefix)
    fname = glue::glue('revscores-{prefix_for_fname}.csv')
    readr::write_csv(out, fname)
    cli::cli_alert_success("Saved file {.val {fname}} to working directory.")
  } else {
    out
  }
}
