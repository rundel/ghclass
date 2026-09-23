github_api_team_add = function(
  org, team_slug, repo,
  permission = c("pull", "push", "admin", "maintain", "triage")
){
  permission = match.arg(permission)

  ghclass_api_v3_req(
    endpoint = "PUT /orgs/:org/teams/:team_slug/repos/:owner/:repo",
    org = org,
    team_slug = team_slug,
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    permission = permission
  )
}


#' @rdname repo_user
#' @param team Character. Slug or name of team to add.
#' @param team_type Character. Either "slug" if the team names are slugs or "name" if full team names are provided.
#' @export
repo_add_team = function(
  repo, team,
  permission = c("push", "pull", "admin", "maintain", "triage"),
  team_type = c("name", "slug")
) {
  arg_is_chr(repo, team, allow_empty = FALSE)
  permission = match.arg(permission)
  team_type = match.arg(team_type)

  org = unique(get_repo_owner(repo))

  if (length(org) != 1) {
    cli_stop("Permissions can only be changed for one organization at a time. ",
             "Requested orgs: {.val {org}}")
  }

  repo = unique(repo)
  team = unique(team)

  d = tibble::tibble(team, repo)
  d = dplyr::distinct(d)

  if (team_type == "name")
    d[["slug"]] = team_slug_lookup(org, d[["team"]])
  else
    d[["slug"]] = d[["team"]]

  check_team_slug(d[["slug"]])

  res = status_scope(
    "Adding teams to repos", nrow(d),
    done = "Gave {n_ok} of {total} team{?s} {.val {permission}} access to repos",
    purrr::pmap(
      d,
      function(team, repo, slug) {
        if (is.na(slug)) {
          status_fail("Team {.val {team}} does not exist in org {.val {org}}.")
          return()
        }

        res = purrr::safely(github_api_team_add)(
          org = org,
          team_slug = slug,
          repo = repo,
          permission = permission
        )

        status_msg(
          res,
          "Team {.val {slug}} given {.val {permission}} access to repo {.val {repo}}",
          "Failed to give team {.val {slug}} {.val {permission}} access to repo {.val {repo}}."
        )
      }
    )
  )

  invisible(res)
}

#' @rdname repo_user
#' @export
#'
repo_team_permission = function(
  repo, team,
  permission = c("push", "pull", "admin", "maintain", "triage"),
  team_type = c("name", "slug")
) {
  arg_is_chr(repo, team, allow_empty = FALSE)
  permission = match.arg(permission)
  team_type = match.arg(team_type)

  repo_add_team(
    repo = repo, team = team, permission = permission, team_type = team_type
  )
}
