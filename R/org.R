#' @export
get_org_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE)
{
  .Depricated("get_repos")
  get_org_repos(org, filter, exclude, full_repo)
}

#' @export
get_org_members = function(org, filter=NULL, exclude=FALSE)
{
  .Depricated("get_members")
  get_members(org, filter, exclude)
}

#' @export
get_org_teams = function(org, filter=NULL, exclude=FALSE)
{
  .Deprecated("get_teams")
  get_teams(org, filter, exclude)
}

#' @export
create_org_teams = function(org, teams=character(), privacy = c("closed","secret"),
                            verbose=TRUE, delay=0.2)
{
  .Deprecated("create_teams")
  create_teams(org, teams, privacy, vebose, delay)
}

#' @export
add_org_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  .Deprecated("add_team_member")
  add_team_member(org, users, teams, create_missing_teams, verbose, delay)
}


#' @export
get_org_members = function(org, filter=NULL, exclude=FALSE)
{
  .Depricated("get_members")
  get_members(org, filter, exclude)
}

#' @export
create_org_teams = function(org, teams=character(), privacy = c("closed","secret"),
                            verbose=TRUE, delay=0.2)
{
  .Deprecated("create_teams")
  create_teams(org, teams, privacy, vebose, delay)
}

#' @export
add_org_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  .Deprecated("add_team_member")
  add_team_member(org, users, teams, create_missing_teams, verbose, delay)
}


#' @export
get_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE)
{
  res = gh("GET /orgs/:org/repos", org = org, .token=get_github_token(), .limit=get_github_api_limit()) %>%
    map_chr("name")

  if (!is.null(filter))
  {
    subset = res %>% str_detect(filter)

    if (exclude)
      res = res[!subset]
    else
      res = res[subset]
  }

  if (length(res) == 0)
    stop("No repos found in ", org, " matching ", filter)

  if (full_repo)
    paste0(org,"/",res)
  else
    res
}

#' @export
get_members = function(org, filter=NULL, exclude=FALSE)
{
  res = gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_github_api_limit()) %>%
    map_chr("login")

  if (!is.null(filter)) {
    subset = res %>% str_detect(filter)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
}

#' @export
get_pending_members = function(org, filter=NULL, exclude=FALSE)
{
  res = gh("GET /orgs/:org/invitations", org=org,
           .token=get_github_token(), .limit=get_github_api_limit()) %>%
    map_chr("login")

  if (!is.null(filter)) {
    subset = res %>% str_detect(filter)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
}

#' @export
get_team_repos = function(org, teams = get_teams(org))
{
  stopifnot(length(org) == 1)

  if (is.character(teams))
  {
    org_teams = get_teams(org)

    sub = teams %in% org_teams$name
    match = filter(org_teams, name %in% teams)

    if (nrow(teams) != length(orig_teams))
      stop("Unable to find teams: ", paste(teams[sub], collapse=", "))

    teams = match
  }

  map2_df(
    teams$name, teams$id,
    function(team, id) {
      res = gh(
        "GET /teams/:id/repos", id=id,
        .token=get_github_token(), .limit=get_github_api_limit()
      )

      data_frame(
        team = team,
        repo = map_chr(res, "full_name")
      )
    }
  )
}

#' @export
get_team_members = function(org, teams = get_teams(org))
{
  stopifnot(length(org) == 1)

  if (is.character(teams))
  {
    org_teams = get_teams(org)

    sub = teams %in% org_teams$name
    match = filter(org_teams, name %in% teams)

    if (nrow(teams) != length(orig_teams))
      stop("Unable to find teams: ", paste(teams[sub], collapse=", "))

    teams = match
  }
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
get_teams = function(org, filter=NULL, exclude=FALSE)
{
  stopifnot(length(org)==1)
  stopifnot(length(filter)<=1)
  stopifnot(length(exclude)==1)

  res = gh("/orgs/:org/teams", org=org, .token=get_github_token(), .limit=get_github_api_limit())

  teams = data.frame(
    name = map_chr(res, "name"),
    id =  map_int(res, "id"),
    stringsAsFactors = FALSE
  )

  if (!is.null(filter))
  {
    subset = str_detect(teams$name, filter)

    if (exclude)
      teams = teams[!subset,]
    else
      teams = teams[subset,]
  }

  return(teams)
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
clean_usernames = function(usernames)
{
  usernames %<>%
    str_trim() %>%
    {.[. != ""]}
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
