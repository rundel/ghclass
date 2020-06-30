missing_team = function(team, id, org) {
  if (is.na(id)) {
    cli::cli_alert_danger()
    TRUE
  } else {
    FALSE
  }
}

team_slug_lookup = function(org, name) {
  dplyr::left_join(
    tibble::tibble(name = name),
    org_team_details(org),
    by = "name"
  )[["slug"]]
}

is_slug = function(name) {
  # a slug consists of solely lowercase alphanumeric characters, dashes and underscores,
  # without 2 or more dashes in a row (sequences of underscores are allowed).
  # Furthermore, a slug cannot start or end with a hyphen.
  !grepl("[^a-z0-9_-]+|-{2,}|^-|-$", name)
}

check_team_slug = function(name, quiet = FALSE) {
  r = is_slug(name)

  if(!quiet & sum(r) != length(r))
    cli::cli_alert_warning(
      "The following team names do not appear to be valid slugs: {.val {name[!r]}}"
    )

  all(r)
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


