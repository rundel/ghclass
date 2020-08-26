# Reads roster file
peer_roster_process = function(roster) {
  UseMethod("peer_roster_process", roster)
}

peer_roster_process.default = function(roster) {
  cli_stop("{.field roster} must be a data frame or a character path to a csv file.")
}

peer_roster_process.character = function(roster) {
  arg_is_chr_scalar(roster)

  if (!fs::file_exists(roster))
    cli_stop("Cannot locate file {.file {roster}}")

  df = readr::read_csv(roster, col_types = readr::cols())
  peer_roster_process(df)
}

peer_roster_process.data.frame = function(df) {
  df = purrr::modify_if(df, is.factor, as.character)
  df = tibble::as_tibble(df)

  # Checking if roster is in the right format

  if (!peer_roster_valid(df)) {
    cli_stop("Peer review roster did not have the expected format (please see {.fun peer_roster_create}'s documentation for details).")
  }

  df
}




# Checks whether necessary column names are present
peer_roster_valid = function(roster) {
  # Check user and user_random colnames
  req_cols = c("user", "user_random")
  missing = !req_cols %in% names(roster)

  if (any(missing))
    cli::cli_alert_danger("Roster must contain column{?s} {.val {req_cols[missing]}}")


  rev_cols = grepl("^rev[0-9]+$", names(roster))
  if (!any(rev_cols)) {
    cli::cli_alert_danger(
      "Roster must contain at least one column with a reviewer number (i.e. a column starting with {.val \"rev\"})."
    )
  }

  return(!(any(missing) | !any(rev_cols)))
}


# Expand roster to make mapping over repos easier
peer_roster_expand = function(org, roster, prefix = "", suffix = "",
                              prefix_review = "", suffix_review = "") {
  arg_is_chr_scalar(org, prefix, suffix, prefix_review, suffix_review)

  rdf = peer_roster_process(roster)

  out = peer_roster_format_cols(org, rdf, prefix, suffix, prefix_review, suffix_review)

  out
}

peer_roster_format_cols = function(org, roster, prefix, suffix, prefix_review, suffix_review) {
  purrr::map_dfr(
    roster[["user"]],
    function(x) {
      tibble::tibble(
        aut = x,
        aut_random = as.character(roster[["user_random"]][roster[["user"]] == x]),
        repo_aut = paste0(org, "/", prefix, x, suffix),
        rev = peer_get_rev(x, roster, "rev"),
        rev_random = peer_get_rev(x, roster, "rev_random"),
        rev_no = peer_get_rev(x, roster, "rev_no"),
        repo_rev = paste0(org, "/", prefix, rev, suffix),
        repo_rev_review = paste0(org, "/", prefix_review, rev, suffix_review),
      )
    }
  )
}
