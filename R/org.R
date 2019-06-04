github_api_get_repos = function(org) {
  stopifnot(length(org)==1)
  gh("GET /orgs/:org/repos", org = org, .token=get_github_token(), .limit=get_github_api_limit())
}

#' Get organization repos
#'
#' \code{get_repos} returns a (filtered) vector of repos belonging to a GitHub organization.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#' @param full_repo logical, should the full repo name be returned (e.g. \code{org/repo} instead of just \code{repo})
#'
#' @examples
#' \dontrun{
#' get_repos("ghclass")
#' get_repos("ghclass", "hw1-")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = purrr::map_chr(github_api_get_repos(org), "name")
  res = filter_results(res, filter, exclude)

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
}


github_api_get_members = function(org) {
  stopifnot(length(org)==1)
  gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_github_api_limit())
}


#' Get organization members
#'
#' \code{get_members} returns a (filtered) vector of organization memebers.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_members("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_members = function(org, filter=NULL, exclude=FALSE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = purrr::map_chr(github_api_get_members(org), "login")

  filter_results(res, filter, exclude)
}



github_api_get_invitations = function(org) {
  gh("GET /orgs/:org/invitations", org=org,
     .token=get_github_token(), .limit=get_github_api_limit())
}

#' Get pending organization members
#'
#' \code{get_pending_members} returns a (filtered) vector of pending organization memebers.
#'
#' @param org character, name of the GitHub organization.
#' @param filter character, regex pattern for matching (or excluding) repos.
#' @param exclude logical, should entries matching the regex be excluded or included.
#'
#' @examples
#' \dontrun{
#' get_pending_members("ghclass")
#' }
#'
#' @family github organization related functions
#'
#' @export
#'
get_pending_members = function(org, filter=NULL, exclude=FALSE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = github_api_get_invitations(org)
  res = purrr::map(res, "login")
  res = as.character(unlist(res))

  filter_results(res, filter, exclude)
}




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
#' @family github organization related functions
#'
#' @export
#'

#' @export
get_teams = function(org, filter=NULL, exclude=FALSE) {
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)

  res = github_api_get_teams(org)

  teams = if (empty_result(res)) {
    tibble::data_frame(
      team = character(),
      id   =  integer()
    )
  } else {
    tibble::data_frame(
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
    usethis::ui_stop(paste(
      "Unable to find team(s):", missing
    ))
  }

  org_teams[org_teams$team %in% teams,]
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
get_team_repos = function(org, team = get_teams(org))
{
  stopifnot(length(org) == 1)

  if (is.character(team))
    team = get_specific_teams(org, team)

  stopifnot(all(c("team","id") %in% names(team)))

  purrr::pmap_df(
    team,
    function(team, id) {
      res = gh(
        "GET /teams/:id/repos", id=id,
        .token=get_github_token(), .limit=get_github_api_limit()
      )

      if (empty_result(res)) {
        tibble::data_frame(
          team = character(),
          repo = character()
        )
      } else {
        tibble::data_frame(
          team = team,
          repo = purrr::map_chr(res, "full_name")
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
#' @family github organization related functions
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
    tibble::data_frame(team = character(), github = character(), pending = logical())
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
        tibble::data_frame(
          team = character(),
          github = character(),
          pending = logical()
        )
      } else {
        tibble::data_frame(
          team = team,
          github = purrr::map_chr(res, "login"),
          pending = FALSE
        )
      }
    }
  )

  tibble::as_data_frame( rbind(cur, pend) )
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
#' @family github organization related functions
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
        tibble::data_frame(
          team = character(),
          github = character()
        )
      } else {
        tibble::data_frame(
          team = team,
          github = purrr::map_chr(res, "login")
        )
      }
    }
  )
}




#' Create team(s)
#'
#' \code{create_team} creates teams in your organization
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
#' @family github organization related functions
#'
#' @export
create_team = function(org, team = character(), privacy = c("closed","secret"), verbose = TRUE)
{
  stopifnot(!missing(org))

  team = as.character(team)
  privacy = match.arg(privacy)

  org_teams = get_teams(org)

  purrr::walk(
    unique(team),
    function(team) {

      if (team %in% org_teams[["team"]]) {
        if (verbose)
          message("Skipping ", team, ", already exists for ", org, " ...")
        return()
      }

      if (verbose)
        message("Adding team ", team, " to ", org, " ...")

      res = safe_gh(
        "POST /orgs/:org/teams",
         org=org, name=team, privacy=privacy,
        .token=get_github_token()
      )

      check_result(
        res,
        sprintf("Failed to create team %s.", team),
        verbose
      )
    }
  )
}

#' @export
rename_team = function(org, cur_team, new_team) {

  stopifnot(length(org) == 1)

  org_teams = get_teams(org)
  team_id_lookup = setNames(org_teams[["id"]], org_teams[["team"]])

  purrr::pwalk(
    list(cur_team, new_team),
    function(cur_team, new_team) {

      if (verbose)
        message("Renaming team ", cur_team, " to ", new_team, " ...")


      res = safe_gh("PATCH /teams/:team_id",
                    team_id = team_id_lookup[cur_team],
                    name = new_team,
                    .token=get_github_token())

      check_result(
        res,
        sprintf("Failed to rename team %s to %s.", repo, new_name),
        verbose
      )
    }
  )
}


#' @export
add_team_member = function(org, user, team, create_missing_teams=FALSE, verbose=TRUE)
{
  stopifnot(!missing(org))
  stopifnot(is.character(user) & length(user) >=1)
  stopifnot(is.character(team) & length(team) >=1)

  info = tibble::data_frame(
    user,
    team
  )

  org_teams = get_teams(org)

  new_teams = setdiff(unique(team), org_teams[["team"]])
  if (length(new_teams) != 0 & create_missing_teams) {
    create_team(org, new_teams, verbose=verbose)
    org_teams = get_teams(org)
  }

  info = merge(
    info, org_teams,
    by = "team", all.x = TRUE
  )

  missing_teams = is.na(info[["id"]])
  if (any(missing_teams))
    stop("Team(s) ", paste(new_teams,collapse=", "), " do(es) not exist in ", org, ".", call. = FALSE)

  purrr::pwalk(
    info,
    function(user, team, id) {

      if (verbose)
        message("Adding ", user, " to team ", team, " ...")

      res = safe_gh(
        "PUT /teams/:id/memberships/:username",
        id=id, username=user, role="member",
        .token=get_github_token()
      )

      check_result(res, sprintf("Failed to add %s to %s in %s.", user, team, org), verbose)
    }
  )
}




#' @export
check_user_exists = function(user)
{
  check_user = function(user) {
    gh("/users/:username", username=user, .token=get_github_token())
    TRUE
  }

  purrr::map_lgl(user, purrr::possibly(check_user, FALSE))
}


#' Invite user(s)
#'
#' \code{invite_user} invites users to your organization
#'
#' @param org character, name of the GitHub organization.
#' @param user character or data frame, listing one or more users
#'
#' @examples
#' \dontrun{
#' users = c("Alice","Bob","Carol","Dave","Eve")
#' invite_user("ghclass", users)
#' }
#'
#' @family github organization related functions
#'
#' @export
invite_user = function(org, user, verbose=TRUE, exclude_pending = FALSE)
{
  stopifnot(length(org) == 1)

  user = tolower(user)
  members = tolower(get_members(org))
  pending = tolower(get_pending_members(org))

  need_invite = setdiff(user, c(members, pending))

  purrr::walk(
    need_invite,
    function(user) {

      if (verbose)
        message("Adding ", user, " to ", org, " ...")


      res = safe_gh(
        "PUT /orgs/:org/memberships/:username",
        org=org, username=user, role="member",
        .token=get_github_token()
      )

      fail = sprintf("Inviting %s to %s failed.", user, org)
      check_result(res, fail, verbose=verbose)
    }
  )
}
