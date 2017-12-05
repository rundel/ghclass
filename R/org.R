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

  if (full_repo)
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

  res = map_chr(
    gh("GET /orgs/:org/invitations", org=org, .token=get_github_token(), .limit=get_github_api_limit()),
    "login"
  )

  if (!is.null(filter)) {
    subset = grepl(filter,res)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
}

#' Get organization teams
#'
#' \code{get_teams} returns a (filtered) data frame organization teams and their unique ids.
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

  res = gh("/orgs/:org/teams", org=org, .token=get_github_token(), .limit=get_github_api_limit())

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
  {
    org_teams = get_teams(org)

    sub = teams %in% org_teams$name
    match = org_teams[org_teams$name %in% teams,]

    if (sum(sub) != length(teams))
      stop("Unable to find teams: ", paste(teams[!sub], collapse=", "))

    teams = match
  }

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
  {
    org_teams = get_teams(org)

    sub = teams %in% org_teams$name
    match = org_teams[org_teams$name %in% teams,]

    if (sum(sub) != length(teams))
      stop("Unable to find teams: ", paste(teams[!sub], collapse=", "))

    teams = match
  }

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
create_teams = function(org, teams=character(), privacy = c("closed","secret"),
                            verbose=TRUE, delay=0.2)
{
  stopifnot(!missing(org))
  teams = as.character(teams)
  privacy = match.arg(privacy)

  for(team in teams)
  {
    if (verbose)
      cat("Adding", team, "...\n")

    gh("POST /orgs/:org/teams",
       org=org, name=team, privacy=privacy,
       .token=get_github_token())

    Sys.sleep(delay)
  }
}

#' @export
add_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  stopifnot(!missing(org))
  stopifnot(is.character(users) & length(users) >=1)
  stopifnot(is.character(teams) & length(teams) >=1)
  stopifnot(length(users)==length(teams) | length(teams) == 1)

  info = data.frame(users, teams, stringsAsFactors = FALSE)

  org_teams = get_teams(org)

  new_teams = setdiff(unique(teams), org_teams$name)
  if (length(new_teams) != 0)
  {
    if (create_missing_teams) {
      create_teams(org=org, new_teams, verbose=verbose)
      org_teams = get_teams(org)
    } else {
      stop("Team(s) ", paste(new_teams,collapse=", "), " do(es) not exist in ", org)
    }
  }


  teams = left_join(
    data.frame(name = teams, stringsAsFactors = FALSE),
    org_teams,
    by = "name")

  for(i in seq_along(users))
  {
    if (verbose)
      cat("Adding ", users[i], " to ", teams$name[i], " ...\n", sep="")

    gh("PUT /teams/:id/memberships/:username",
       id=teams$id[i], username=users[i],
       role="member",
       .token=get_github_token())

    Sys.sleep(delay)
  }
}




#' @export
check_users = function(users)
{
  users %>%
    clean_usernames() %>%
    map(~ try( {gh("/users/:username", username=., .token=get_github_token())}, silent=TRUE)) %>%
    map_lgl(~ !any(class(.) == "try-error"))
}

#' @export
invite_users = function(org, users, verbose=TRUE, exclude_pending = FALSE)
{
  users = users %>% clean_usernames() %>% tolower()
  current = get_members(org) %>% tolower() %>% intersect(users)
  pending = get_pending_members(org) %>% tolower() %>% intersect(users)

  need_invite = setdiff(users, c(current, pending))

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
  if (verbose)
    sprintf("Current status: %i invited, %i pending, %i joined.",
            length(need_invite), length(pending), length(current)) %>%
    cat("\n", sep="")
}
