#' Collect scores from rating forms
#'
#' The `peer_score_rating()` function collects score information from the YAML of a rating form within authors' repositories. It outputs a new .csv file, with rows specifying individual question scores for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param form_rating Character. File name of rating feedback form (must be .Rmd document).
#' @param double_blind Logical. Specifies whether review is conducted double-blind (i.e. neither reviewer nor author can identify each other), or single-blind (i.e. authors remain anonymous but reviewer identities are revealed). If `double_blind = TRUE`, reviewer folders are identified by the anonymized user IDs in the roster's `user_random` column. If `double_blind = FALSE`, reviewer folders are identified by the original user names. Defaults to `TRUE`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @examples
#' \dontrun{
#' peer_score_rating(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' form_rating = "hw2_rating.Rmd",
#' double_blind = TRUE,
#' prefix = prefix)
#' }
#'
#' @importFrom rlang .data
#' @family peer review functions
#'
#' @export
#'
peer_score_rating = function(org, roster, form_rating, double_blind = TRUE,
                             prefix = "", suffix = "", write_csv = TRUE) {
  # Checks
  arg_is_chr_scalar(org, prefix, suffix, form_rating)
  arg_is_lgl(double_blind, write_csv)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", form_rating)) {
    cli_stop("{.field form_rating} must be an {.file .Rmd} file.")
  }

  prefix_review = format_rev(prefix, suffix)[['prefix_review']]
  suffix_review = format_rev(prefix, suffix)[['suffix_review']]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)

  out_temp = purrr::map_dfr(
    seq_len(nrow(rdf)),
    function(x) {
      repo = as.character(rdf[x, 'repo_aut'])
      if (double_blind) {
        ghpath = glue::glue("{as.character(rdf[x, 'rev_no'])}/{form_rating}")
      } else {
        ghpath = glue::glue("{as.character(rdf[x, 'rev'])}/{form_rating}")
      }
      rev_no = as.character(rdf[x, 'rev_no'])

      feedback = purrr::safely(repo_get_file)(repo = repo, path = ghpath)

      if (succeeded(feedback)) {
        tc = textConnection(feedback[['result']])
        scores = rmarkdown::yaml_front_matter(tc)[['params']]
        scores[scores == "[INSERT SCORE]"] = NA
        scores = purrr::map_chr(scores, ~ gsub("[][]", "", .x))

        inp = stats::setNames(c(as.character(rdf[x, 'rev']), rev_no, scores),
                              c("user", "rev_no", paste0("c", 1:length(scores))))

        tibble::as_tibble(as.list(inp))
      } else {
        cli::cli_alert_danger("Cannot locate file {.val {ghpath}} on repo {.val {repo}}.")
      }
    }
  )

  # Getting data frame in right format
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
    fname = glue::glue("{autscores}_{fs::path_file(roster)}")
    readr::write_csv(out, fname)
    cli::cli_alert_success("Saved file {.val {fname}} to working directory.")
  } else {
    out
  }
}
