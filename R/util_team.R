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

