github_api_get_teams = function(org) {
  gh("GET /orgs/:org/teams", org=org,
     .token=get_github_token(), .limit=get_github_api_limit())
}

#' Get organization teams
#'
#' \code{get_teams} returns a (filtered) data frame of teams in the organization with columns for
#' their names (`name`) and their unique ids (`id`).
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_team_repos("ghclass",c("team01","team02"))
#' }
#'
#' @family github organization team related functions
#'
#' @export
#'
get_teams = function(org, filter=NULL, exclude=FALSE) {
  arg_is_chr_scalar(org)
  arg_is_lgl_scalar(exclude)
  stopifnot(length(filter)<=1)

  res = github_api_get_teams(org)

  teams = if (empty_result(res)) {
    tibble::tibble(
      team = character(),
      id   =  integer()
    )
  } else {
    tibble::tibble(
      team = purrr::map_chr(res, "name"),
      id   = purrr::map_int(res, "id")
    )
  }

  filter_results(teams, "team", filter, exclude)
}


get_specific_teams = function(org, teams, strict = TRUE) {
  org_teams = get_teams(org)

  sub = teams %in% org_teams[["team"]]
  if (sum(sub) != length(teams) & strict) {
    missing = paste(teams[!sub], collapse=", ")
    usethis::ui_stop( paste0(
      "Team(s) {usethis::ui_value(missing)} do not exist ",
      "in org {usethis::ui_value(org)}."
    ) )
  }

  org_teams[org_teams$team %in% teams,]
}

team_id_lookup = function(d, org_teams) {
  d = merge(
    org_teams, d,
    by = "team", all.y = TRUE
  )

  missing_teams = d[["team"]][is.na(d[["id"]])]

  # This should not ever happen, get_specific_team should handle this
  stopifnot(length(missing_teams) == 0)

  d
}


github_api_get_team_repos = function(team_id) {
  gh::gh(
    "GET /teams/:id/repos", id=team_id,
    .token=get_github_token(), .limit=get_github_api_limit()
  )
}

#' Get teams' repos
#'
#' \code{get_team_repos} returns a (filtered) data frame of teams and their repos.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#'
#' @examples
#' \dontrun{
#' get_team_repos("ghclass",c("team01","team02"))
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_team_repos = function(org, team)
{
  arg_is_chr_scalar(org)
  arg_is_chr(team)

  team = get_specific_teams(org, team)

  purrr::pmap_dfr(
    team,
    function(team, id) {
      res = purrr::safely(github_api_get_team_repos)(id)

      if (succeeded(res) & !empty_result(result(res))) {
        tibble::tibble(
          team = team,
          repo = purrr::map_chr(result(res), "full_name")
        )
      } else {
        tibble::tibble(
          team = character(),
          repo = character()
        )
      }
    }
  )
}

#' Get team members
#'
#' \code{get_team_members} returns a data frame of teams and their members.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#' @param get_pending include pending team members
#'
#' @examples
#' \dontrun{
#' get_team_members("ghclass",c("team01","team02"))
#' }
#'
#' @family github organization team related functions
#'
#' @export
#'
get_team_members = function(org, team = get_teams(org), get_pending = TRUE)
{
  stopifnot(length(org) == 1)

  pend = if (get_pending) {
    res = get_pending_team_members(org, team)
    res[["pending"]] = rep(FALSE, nrow(res))
    res
  } else {
    tibble::tibble(team = character(), github = character(), pending = logical())
  }

  if (is.character(team))
    team = get_specific_teams(org, team)

  stopifnot(all(c("team","id") %in% names(team)))

  cur = purrr::pmap_df(
    team,
    function(team, id) {
      res = gh(
        "GET /teams/:id/members",
        id=id, role = "all",
        .token=get_github_token(), .limit=get_github_api_limit()
      )

      if (empty_result(res)) {
        tibble::tibble(
          team = character(),
          github = character(),
          pending = logical()
        )
      } else {
        tibble::tibble(
          team = team,
          github = purrr::map_chr(res, "login"),
          pending = FALSE
        )
      }
    }
  )

  tibble::as_tibble( rbind(cur, pend) )
}

#' Get pending team members
#'
#' \code{get_pending_team_members} returns a data frame of pending team members.
#'
#' @param org character, name of the GitHub organization.
#' @param team character or data frame, listing one or more team
#'
#' @examples
#' \dontrun{
#' get_pending_team_members("ghclass",c("team01","team02"))
#' }
#'
#' @family github organization team related functions
#'
#' @export
#'
get_pending_team_members = function(org, team = get_teams(org))
{
  stopifnot(length(org) == 1)

  if (is.character(team))
    team = get_specific_teams(org, team)

  stopifnot(all(c("team","id") %in% names(team)))

  purrr::pmap_df(
    team,
    function(team, id) {
      res = gh(
        "GET /teams/:id/invitations",
        id=id,
        .token=get_github_token(), .limit=get_github_api_limit()
      )

      if (empty_result(res)) {
        tibble::tibble(
          team = character(),
          github = character()
        )
      } else {
        tibble::tibble(
          team = team,
          github = purrr::map_chr(res, "login")
        )
      }
    }
  )
}


github_api_create_team = function(org, name, privacy) {
  gh(
    "POST /orgs/:org/teams",
    org=org, name=name, privacy=privacy,
    .token=get_github_token()
  )
}

#' Create team(s)
#'
#' \code{create_team} creates teams in your GitHub organization
#'
#' @param org character, name of the GitHub organization
#' @param team character, listing one or more teams
#' @param privacy character, level of privacy of teams, closed (visible to all
#' members of the organization) or secret (only visible to organization owners
#' and members of a team), default is closed
#'
#' @examples
#' \dontrun{
#' create_team("ghclass",c("team01","team01"))
#' }
#'
#' @family github organization team related functions
#'
#' @export
create_team = function(org, team, privacy = c("closed","secret")) {
  team = unique(as.character(team))
  privacy = match.arg(privacy)

  org_teams = get_teams(org)[["team"]]

  new_teams = setdiff(team, org_teams)
  existing_teams = intersect(team, org_teams)

  if (length(existing_teams) > 0)
    usethis::ui_info("Skipping existing teams: {usethis::ui_value(existing_teams)}.")

  purrr::walk(
    new_teams,
    function(team) {
      res = purrr::safely(github_api_create_team)(
        org=org, name=team, privacy=privacy
      )

      status_msg(
        res,
        glue::glue("Added team {usethis::ui_value(team)} to org {usethis::ui_value(org)}."),
        glue::glue("Failed to add team {usethis::ui_value(team)} to org {usethis::ui_value(org)}."),
      )
    }
  )
}

github_api_rename_team = function(id, new_name) {
  gh(
    "PATCH /teams/:team_id",
    team_id = id,
    name = new_name,
    .token=get_github_token()
  )
}


#' Rename existing team(s)
#'
#' \code{rename_team} renames an existing team within the given GitHub organization.
#'
#' @param org character, name of the GitHub organization
#' @param team character, one or more existing team names
#' @param new_team character, one or more new team names
#'
#' @examples
#' \dontrun{
#' rename_team("ghclass-test", "hw1-team01", "hw01-team01")
#' }
#'
#' @family github organization team related functions
#'
#' @export
rename_team = function(org, team, new_team) {
  stopifnot(length(org) == 1)

  d = tibble::tibble(
    team = team,
    new_team = new_team
  )

  d = team_id_lookup(d, get_teams(org))

  purrr::pwalk(
    d,
    function(team, id, new_team) {
      res = purrr::safely(github_api_rename_team)(id, new_team)

      status_msg(
        res,
        glue::glue("Renamed team {usethis::ui_value(team)} to {usethis::ui_value(new_team)}."),
        glue::glue("Failed to rename team {usethis::ui_value(team)} to {usethis::ui_value(new_team)}.")
      )
    }
  )
}


github_api_add_team_member = function(team_id, username) {
  gh(
    "PUT /teams/:id/memberships/:username",
    id=team_id, username=username, role="member",
    .token=get_github_token()
  )
}

#' Add Members to an Organizaton's Team(s)
#'
#' \code{add_team_member} add members to GitHub Organization Teams.
#'
#' @param org character, name of the GitHub organization
#' @param user character, one or more usernames to invite
#' @param team character, one or more team names
#' @param create_missing_teams logical, if a team does not already exist
#' should it be added to the GitHub organization.
#'
#' @examples
#' \dontrun{
#' add_team_member("ghclass-test", "rundel", c("hw1-team01","hw1-team02"))
#' }
#'
#' @family github organization team related functions
#'
#' @export
add_team_member = function(org, user, team, create_missing_teams = TRUE) {
  stopifnot(!missing(org))

  stopifnot(is.character(user) & length(user) >=1)
  stopifnot(is.character(team) & length(team) >=1)
  stopifnot(length(create_missing_teams) == 1)

  d = tibble::tibble(user, team)

  org_teams = get_teams(org)
  new_teams = setdiff(unique(team), org_teams[["team"]])

  if (length(new_teams) > 0 & create_missing_teams) {
    create_team(org, new_teams)
    org_teams = get_teams(org)
  }

  d = team_id_lookup(d, org_teams)

  purrr::pwalk(
    d,
    function(user, team, id) {
      res = purrr::safely(github_api_add_team_member)(id, user)

      status_msg(
        res,
        glue::glue("Added {usethis::ui_value(user)} to team {usethis::ui_value(team)}."),
        glue::glue("Failed to add {usethis::ui_value(user)} to team {usethis::ui_value(team)}.")
      )
    }
  )
}
