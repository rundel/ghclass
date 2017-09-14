get_org_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE)
{
  .Depricated("get_repos")
  get_org_repos(org, filter, exclude, full_repo)
}

get_org_members = function(org, filter=NULL, exclude=FALSE)
{
  .Depricated("get_members")
  get_members(org, filter, exclude)
}

get_org_teams = function(org, filter=NULL, exclude=FALSE)
{
  .Deprecated("get_teams")
  get_teams(org, filter, exclude)
}

create_org_teams = function(org, teams=character(), privacy = c("closed","secret"),
                            verbose=TRUE, delay=0.2)
{
  .Deprecated("create_teams")
  create_teams(org, teams, privacy, vebose, delay)
}

add_org_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  .Deprecated("add_team_member")
  add_team_member(org, users, teams, create_missing_teams, verbose, delay)
}



get_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE)
{
  .Depricated("get_repos")
  get_repos(org, filter, exclude, full_repo)
}

get_org_members = function(org, filter=NULL, exclude=FALSE)
{
  .Depricated("get_members")
  get_members(org, filter, exclude)
}

get_org_teams = function(org, filter=NULL, exclude=FALSE)
{
  .Deprecated("get_teams")
  get_teams(org, filter, exclude)
}

create_org_teams = function(org, teams=character(), privacy = c("closed","secret"),
                            verbose=TRUE, delay=0.2)
{
  .Deprecated("create_teams")
  create_teams(org, teams, privacy, vebose, delay)
}

add_org_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  .Deprecated("add_team_member")
  add_team_member(org, users, teams, create_missing_teams, verbose, delay)
}



get_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE)
{
  res = gh("GET /orgs/:org/repos", org = org, .token=get_github_token(), .limit=get_api_limit()) %>%
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

get_members = function(org, filter=NULL, exclude=FALSE)
{
  res = gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_api_limit()) %>%
    map_chr("login")

  if (!is.null(filter)) {
    subset = res %>% str_detect(filter)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
}

get_pending_members = function(org, filter=NULL, exclude=FALSE)
{
  res = gh("GET /orgs/:org/invitations", org=org,
           .token=get_github_token(), .limit=get_api_limit()) %>%
    map_chr("login")

  if (!is.null(filter)) {
    subset = res %>% str_detect(filter)
    if (exclude) res = res[!subset]
    else         res = res[subset]
  }

  res
}


get_teams = function(org, filter=NULL, exclude=FALSE)
{
  teams = gh("/orgs/:org/teams", org=org, .token=get_github_token(), .limit=get_api_limit())

  res = map_int(teams, "id") %>%
    setNames(map(teams, "name"))

  if (!is.null(filter))
  {
    subset = names(res) %>% str_detect(filter)

    if (exclude)
      res = res[!subset]
    else
      res = res[subset]
  }

  return(res)
}

create_teams = function(org, teams=character(), privacy = c("closed","secret"),
                            verbose=TRUE, delay=0.2)
{
  stopifnot(!missing(org))
  teams = as.character(teams)
  privacy = match.arg(privacy)

  for(team in teams)
  {
    if (verbose)
      cat("Adding ", team, "...\n", sep="")

    gh("POST /orgs/:org/teams",
       org=org, name=team, privacy=privacy,
       .token=get_github_token())

    Sys.sleep(delay)
  }
}

add_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  stopifnot(!missing(org))
  stopifnot(is.character(users) & length(users) >=1)
  stopifnot(is.character(teams) & length(teams) >=1)
  stopifnot(length(users)==length(teams) | length(teams) == 1)

  info = data.frame(users, teams, stringsAsFactors = FALSE)

  team_ids = get_teams(org)

  new_teams = setdiff(unique(teams), names(team_ids))

  if (length(new_teams) != 0)
  {
    if (create_missing_teams)
      create_org_teams(org=org, new_teams, verbose=verbose)
    else
      stop("Team(s) ",paste(new_teams,collapse=", "), " do(es) not exist for ", org)
  }

  for(i in seq_along(users))
  {
    if (verbose)
      cat("Adding ", users[i], " to ", teams[i], "...\n", sep="")

    gh("PUT /teams/:id/memberships/:username",
       id=team_ids[team], username=users[i],
       role="member",
       .token=get_github_token())

    Sys.sleep(delay)
  }
}

clean_usernames = function(usernames)
{
  usernames %<>%
    str_trim() %>%
    {.[. != ""]}
}

check_users = function(users)
{
  users %>%
    clean_usernames() %>%
    map(~ try( {gh("/users/:username", username=., .token=get_github_token())}, silent=TRUE)) %>%
    map_lgl(~ !any(class(.) == "try-error"))
}

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
