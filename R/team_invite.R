github_api_team_invite = function(org, team_slug, username, role = c("member", "maintainer")) {
  role = match.arg(role)

  ghclass_api_v3_req(
    endpoint = "PUT /orgs/:org/teams/:team_slug/memberships/:username",
    org = org,
    team_slug = team_slug,
    username = username,
    role = role
  )
}

#' @rdname team_members
#' @export
team_invite = function(org, user, team, team_type = c("name", "slug")) {
  arg_is_chr_scalar(org)
  arg_is_chr(user, team)
  team_type = match.arg(team_type)


  if (team_type == "name")
    slug = team_slug_lookup(org, team)
  else
    slug = team

  check_team_slug(slug)

  d = unique(tibble::tibble(user, team, slug))

  res = status_scope(
    "Adding users to teams", nrow(d),
    done = "Added {n_ok} of {total} user{?s} to teams",
    purrr::pmap(
      d,
      function(user, team, slug) {
        if (is.na(slug)) {
          status_fail("Team {.val {team}} does not exist in org {.val {org}}.")
          return(NULL)
        }

        res = purrr::safely(github_api_team_invite)(org, slug, user)

        status_msg(
          res,
          "Added user {.val {user}} to team {.val {team}}.",
          "Failed to add user {.val {user}} to team {.val {team}}."
        )

        result(res)
      }
    )
  )

  invisible(res)
}
