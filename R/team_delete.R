github_api_team_delete = function(org, team_slug) {
  ghclass_api_v3_req(
    endpoint = "DELETE /orgs/:org/teams/:team_slug",
    org = org,
    team_slug = team_slug
  )
}

#' @rdname team
#' @export
#'
team_delete = function(org, team, team_type = c("name", "slug"), prompt = TRUE) {
  arg_is_chr_scalar(org)
  arg_is_chr(team)
  arg_is_lgl_scalar(prompt)
  team_type = match.arg(team_type)

  if (prompt) {
    delete = cli_yeah("This command will delete the following teams permanently: {.val {team}}.")
    if (!delete) {
      return(invisible())
    }
  }

  if (team_type == "name")
    slug = team_slug_lookup(org, team)
  else
    slug = team

  check_team_slug(slug)

  d = tibble::tibble(team, slug)

  res = status_scope(
    "Deleting teams", nrow(d),
    done = "Deleted {n_ok} of {total} team{?s} from org {.val {org}}",
    purrr::pmap(
      d,
      function(team, slug) {
        if (is.na(slug)) {
          status_fail("Team {.val {team}} does not exist in org {.val {org}}.")
          return()
        }

        res = purrr::safely(github_api_team_delete)(org, slug)

        status_msg(
          res,
          "Deleted team {.val {team}} from org {.val {org}}.",
          "Failed to delete team {.val {team}} from org {.val {org}}."
        )
      }
    )
  )

  invisible(res)
}
