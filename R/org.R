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

  res = map_chr(
    gh("GET /orgs/:org/repos", org = org, .token=get_github_token(), .limit=get_github_api_limit()),
    "name"
  )

  if (!is.null(filter)){
    subset = grepl(filter, res)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  if (full_repo & length(res) > 0)
    res = paste0(org,"/",res)

  res
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

  res = map_chr(
    gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_github_api_limit()),
    "login"
  )

  if (!is.null(filter)) {
    subset = grepl(filter, res)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
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

  req = gh("GET /orgs/:org/invitations", org=org, .token=get_github_token(), .limit=get_github_api_limit())

  res = if (req != "") {
    map_chr(req, "login")
  } else {
    character()
  }

  if (!is.null(filter)) {
    subset = grepl(filter,res)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
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

  res = gh("GET /orgs/:org/teams", org=org, .token=get_github_token(), .limit=get_github_api_limit())

  if (length(res) == 1 & all(res == "")) {
    return(
      teams = data.frame(
        name = character(),
        id =  integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  teams = data.frame(
    name = map_chr(res, "name"),
    id =  map_int(res, "id"),
    stringsAsFactors = FALSE
  )

  if (!is.null(filter)) {
    subset = grepl(filter, teams$name)
    if (exclude) teams = teams[!subset,]
    else         teams = teams[subset,]
  }

  teams
}


get_teams_by_list = function(org, teams) {
  org_teams = get_teams(org)

  sub = teams %in% org_teams$name
  if (sum(sub) != length(teams))
    stop("Unable to find team(s): ", paste(teams[!sub], collapse=", "))

  org_teams[org_teams$name %in% teams,]
}


#' Get teams' repos
#'
#' \code{get_team_repos} returns a (filtered) data frame of teams and their repos.
#'
#' @param org character, name of the GitHub organization.
#' @param teams character or data frame, listing one or more team
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
get_team_repos = function(org, teams = get_teams(org))
{
  stopifnot(length(org) == 1)

  if (is.character(teams))
    teams = get_teams_by_list(org, teams)

  stopifnot(all(c("name","id") %in% names(teams)))

  map2_df(
    teams$name, teams$id,
    function(team, id) {
      res = gh(
        "GET /teams/:id/repos", id=id,
        .token=get_github_token(), .limit=get_github_api_limit()
      )
      data.frame(
        team = team,
        repo = map_chr(res, "full_name"),
        stringsAsFactors = FALSE
      )
    }
  )
}

#' Get teams' members
#'
#' \code{get_team_members} returns a data frame of teams and their members.
#'
#' @param org character, name of the GitHub organization.
#' @param teams character or data frame, listing one or more team
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
get_team_members = function(org, teams = get_teams(org))
{
  stopifnot(length(org) == 1)

  if (is.character(teams))
    teams = get_teams_by_list(org, teams)

  stopifnot(all(c("name","id") %in% names(teams)))

  map2_df(
    teams$name, teams$id,
    function(team, id) {
      res = gh(
        "GET /teams/:id/members",
        id=id, role = "all",
        .token=get_github_token(), .limit=get_github_api_limit()
      )
      data_frame(
        team = team,
        member = map_chr(res, "login")
      )
    }
  )
}



#' @export
create_teams = function(org, teams=character(), privacy = c("closed","secret"), verbose=TRUE)
{
  stopifnot(!missing(org))
  teams = as.character(teams)
  privacy = match.arg(privacy)

  for(team in teams)
  {
    if (verbose)
      cat("Creating", team, "...\n")

    try({
      gh("POST /orgs/:org/teams",
         org=org, name=team, privacy=privacy,
        .token=get_github_token())
    })
  }
}

#' @export
add_team_members = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE)
{
  stopifnot(!missing(org))
  stopifnot(is.character(users) & length(users) >=1)
  stopifnot(is.character(teams) & length(teams) >=1)
  stopifnot(length(users)==length(teams) | length(teams) == 1)

  info = data.frame(users, teams, stringsAsFactors = FALSE)

  org_teams = get_teams(org)

  new_teams = setdiff(unique(teams), org_teams$name)
  if (length(new_teams) != 0) {
    if (!create_missing_teams)
      stop("Team(s) ", paste(new_teams,collapse=", "), " do(es) not exist in ", org,".")

    create_teams(org=org, new_teams, verbose=verbose)
    org_teams = get_teams(org)
  }

  teams = merge(
    data.frame(name = teams, stringsAsFactors = FALSE),
    org_teams, all.x = TRUE
  )
  stopifnot(!any(is.na(teams$id)))

  pwalk(
    list(users, teams$name, teams$id),
    function(user, team, id) {
      if (verbose)
        message("Adding ", user, " to ", team, " ...\n", sep="")

      try({
        gh("PUT /teams/:id/memberships/:username",
           id=id, username=user, role="member",
          .token=get_github_token())
      })
    }
  )
}




#' @export
check_users_exist = function(users)
{
  check_user = function(user) {
    gh("/users/:username", username=user, .token=get_github_token())
    TRUE
  }

  map_lgl(users, possibly(check_user, FALSE))
}

#' @export
invite_users = function(org, users, verbose=TRUE, exclude_pending = FALSE)
{
  users = tolower(users)
  members = tolower(get_members(org))
  pending = tolower(get_pending_members(org))

  need_invite = setdiff(users, c(members, pending))

  for(user in need_invite)
  {
    if (verbose)
      cat("Adding ", user, " to ", org, " ...\n", sep="")

    try({
      gh("PUT /orgs/:org/memberships/:username",
         org=org, username=user, role="member",
         .token=get_github_token())
    })
  }
}
