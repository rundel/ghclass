#' @title Add team assignments to a roster
#'
#' @description
#' `team_roster()` adds a team (or repo) name column to a roster data frame by
#' randomly grouping members into teams of a target `size`. Teams may be formed
#' within groups (e.g. lab sections) via `by`, and names are built from a
#' [glue][glue::glue_data] template that can reference any column of `roster`
#' together with the generated `team_id`. When `by` is supplied and no `name` is
#' given, the grouping column(s) are folded into the default name so team names
#' stay unique across groups.
#'
#' @param roster Data frame. Course roster with one row per student.
#' @param size Integer. Target (maximum) team size. Within each group members are
#'   split into `ceiling(n / size)` teams of as-equal-as-possible size. An error
#'   is thrown if a group has fewer than `size` members.
#' @param name Character. A glue template for the team / repo name, used as-is if
#'   supplied. May reference any column of `roster` as well as `team_id` (the
#'   within-group team number). If `NULL` (default) the template is
#'   `"team{team_id}"`, or `"{group}-team{team_id}"` when `by` is set.
#' @param by Character. Optional column name(s) in `roster` to form teams within
#'   (e.g. a lab section). If `NULL` (default) teams are formed across the whole roster.
#' @param col Character. Name of the output column to add. Default `"team"`.
#' @param pad Logical or integer. Zero-padding applied to `team_id` before it is
#'   substituted into `name`. `TRUE` (default) pads to a consistent width based on
#'   the largest team number, `FALSE` leaves it as an integer, or supply an integer
#'   for a fixed width.
#' @param shuffle Logical. Should members be randomly assigned to teams? Default `TRUE`.
#' @param seed Integer. Optional random seed for reproducible assignments.
#'
#' @return The `roster` data frame with `team_id` and `col` columns added.
#'
#' @examples
#' \dontrun{
#' roster = readr::read_csv(system.file("roster.csv", package = "ghclass"))
#'
#' # Teams of 4 across the whole roster -> "team01", "team02", ...
#' team_roster(roster, size = 4)
#'
#' # Teams of 4 within each lab section, group folded into the default name
#' team_roster(roster, size = 4, by = "section", seed = 20250901)
#'
#' # Custom name template (used exactly as written)
#' team_roster(
#'   roster, size = 4, by = "section",
#'   name = "hw1_lab{section}_team{team_id}", seed = 20250901
#' )
#' }
#'
#' @export
#'
team_roster = function(
  roster, size, name = NULL, by = NULL,
  col = "team", pad = TRUE, shuffle = TRUE, seed = NULL
) {
  arg_is_df(roster)
  arg_is_pos_int_scalar(size)
  arg_is_chr_scalar(col)
  arg_is_chr_scalar(name, allow_null = TRUE)
  arg_is_chr(by, allow_null = TRUE)
  arg_is_lgl_scalar(shuffle)
  arg_is_pos_int_scalar(seed, allow_null = TRUE)

  if (is.logical(pad)) arg_is_lgl_scalar(pad) else arg_is_pos_int_scalar(pad)

  missing_cols = setdiff(by, names(roster))
  if (length(missing_cols) > 0)
    cli_stop("Column{?s} {.val {missing_cols}} (from {.arg by}) {?is/are} not present in {.arg roster}.")

  if (is.null(by)) {
    if (nrow(roster) < size)
      cli_stop(
        "Cannot form teams of {.arg size} {.val {size}}: ",
        "the roster has only {nrow(roster)} member{?s}."
      )
  } else {
    counts = dplyr::count(roster, dplyr::across(dplyr::all_of(by)))
    small = counts[counts[["n"]] < size, , drop = FALSE]
    if (nrow(small) > 0) {
      groups = do.call(paste, c(small[by], sep = "/"))
      cli_stop(
        "Cannot form teams of {.arg size} {.val {size}}: ",
        "{cli::qty(groups)} group{?s} ({.val {groups}}) {?has/have} fewer than {size} member{?s}."
      )
    }
  }

  if (is.null(name)) {
    name = if (is.null(by)) {
      "team{team_id}"
    } else {
      paste0(paste0("{", by, "}", collapse = "-"), "-team{team_id}")
    }
  }

  clobber = intersect(c("team_id", col), names(roster))
  if (length(clobber) > 0)
    cli::cli_alert_info("Overwriting existing column{?s}: {.val {clobber}}.")

  assign_ids = function(n) {
    # Build balanced team ids for n members:
    #   ceiling(n / size) -> number of teams needed so none exceeds `size`
    #   rep(., size)      -> repeat the team ids enough times to cover everyone
    #   [seq_len(n)]      -> keep the first n, cycling ids round-robin so team
    #                        sizes never differ by more than one
    ids = rep(seq_len(ceiling(n / size)), size)[seq_len(n)]
    # Shuffle so members are assigned to teams at random, not in roster order
    if (shuffle) ids = sample(ids)
    ids
  }

  do_assign = function() {
    if (is.null(by)) {
      roster[["team_id"]] = assign_ids(nrow(roster))
      roster
    } else {
      roster %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
        dplyr::mutate(team_id = assign_ids(dplyr::n())) %>%
        dplyr::ungroup()
    }
  }

  out = if (is.null(seed)) do_assign() else withr::with_seed(seed, do_assign())

  if (!isFALSE(pad)) {
    width = if (isTRUE(pad)) max(stringr::str_length(out[["team_id"]])) else pad
    out[["team_id"]] = stringr::str_pad(out[["team_id"]], width = width, side = "left", pad = "0")
  }

  out[[col]] = as.character(glue::glue_data(out, name))

  out
}
