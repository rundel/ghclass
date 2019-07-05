missing_team = function(id, org) {
  if (is.na(id)) {
    usethis::ui_oops("Failed to find team {usethis::ui_value(team)} in org {usethis::ui_value(org)}.")
    TRUE
  } else {
    FALSE
  }
}

team_id_lookup = function(x, ...) {
  UseMethod("team_id_lookup", x)
}

team_id_lookup.default = function(x, org) {
  usethis::ui_stop("Unsupported class: {usethis::ui_value(class(x))}.")
}

team_id_lookup.data.frame = function(x, org) {
  team_ids = org_team_ids(org)

  merge(
    team_ids, x,
    by = "team", all.y = TRUE
  )
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
