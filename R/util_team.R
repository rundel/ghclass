missing_team = function(team, id, org) {
  if (is.na(id)) {
    cli::cli_alert_danger("Failed to find team {.val {team}} in org {.val {org}}.")
    TRUE
  } else {
    FALSE
  }
}

team_id_lookup = function(x, ...) {
  UseMethod("team_id_lookup", x)
}

team_id_lookup.default = function(x, org) {
  cli_stop("Unsupported class: {.val {class(x)}}.")
}

team_id_lookup.data.frame = function(x, org) {
  if (nrow(x) == 0) {
    tibble::tibble(
      team = character(),
      id = integer()
    )
  } else {
    team_ids = org_team_ids(org)

    merge(
      team_ids, x,
      by = "team", all.y = TRUE
    )
  }
}

team_id_lookup.character = function(x, org) {
  team_id_lookup(
    tibble::tibble(team = x), org
  )
}

team_id_lookup.NULL = function(x, org) {
  tibble::tibble(
    team = character(),
    id = integer()
  )
}


filter_results.data.frame = function(res, col, pattern = NULL, exclude = FALSE) {
  if (!is.null(pattern)) {
    subset = grepl(pattern, res[[col]])
    if (exclude) res = res[!subset,]
    else         res = res[subset,]
  }
  res
}
