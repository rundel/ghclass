#' Initiate peer review repositories
#'
#' `peer_init()` initiates peer review repositories. It creates a review repository for each user, adds users to their respective repositories, and applies peer review labels to all repositories (i.e. assignment and review repositories).
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `rev*` columns that specify review assignments as values of the vector `user_random`.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param verbose Logical. Whether information about label creation should be given, defaults to `FALSE`. Issues can still be created even labels were not applied to a particular repository.
#' @param show_label_result Logical. Whether a tibble with details on the creation of labels should be returned, defaults to `FALSE`.
#'
#' @examples
#' \dontrun{
#' peer_init(org = "ghclass-test", roster = "roster_test", prefix = "hw2-")
#' }
#'
#' @family peer review functions
#'
#' @export
peer_init = function(org, roster, prefix = "", suffix = "", verbose = FALSE, show_label_result = FALSE) {
  arg_is_chr_scalar(org, prefix, suffix)

  prefix_review = format_rev(prefix, suffix)[["prefix_review"]]
  suffix_review = format_rev(prefix, suffix)[["suffix_review"]]

  rdf = peer_roster_expand(org, roster, prefix, suffix, prefix_review, suffix_review)
  user = unique(rdf[['rev']])

  repo_create(
    org = org,
    name = user,
    prefix = prefix_review,
    suffix = suffix_review
  )
  repo_add_user(glue::glue("{org}/{prefix_review}{user}{suffix_review}"),
                user)

  peer_issue_label_apply(
    org = org,
    repo = unique(c(rdf[['repo_aut']], rdf[['repo_rev_review']])),
    verbose = verbose,
    show_label_result = show_label_result
  )
}
