github_api_team_pending = function(org, team_slug) {
  ghclass_api_v3_req(
    endpoint = "GET /orgs/:org/teams/:team_slug/invitations",
    org = org,
    team_slug = team_slug
  )
}

#' @rdname team_members
#' @export
#'
team_pending = function(org, team = org_teams(org), team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)
  team_type = match.arg(team_type)

  slug = if (team_type == "name") team_slug_lookup(org, team) else team
  check_team_slug(slug)

  status_scope(
    "Retrieving pending members", length(team),
    done = "Retrieved pending members for {n_ok} of {total} team{?s}",
    purrr::map2_dfr(
      team, slug,
      function(team, slug) {
        if (is.na(slug)) {
          status_fail("Team {.val {team}} does not exist in org {.val {org}}.")
          res = NULL
        } else {
          res = purrr::safely(github_api_team_pending)(org, slug)

          status_msg(
            res,
            fail = "Failed to retrieve pending members for team {.val {team}}."
          )
        }

        pending = if (failed(res) | empty_result(res))
          character()
        else
          purrr::map_chr(result(res), "login")

        tibble::tibble(
          team = team,
          slug = slug,
          user = pending
        )
      }
    )
  )
}
