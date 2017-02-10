get_org_repos = function(org, filter=NULL, exclude=FALSE, full_repo=TRUE)
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

  if (full_repo)
    paste0(org,"/",res)
  else
    res
}


get_org_members = function(org, filter=NULL, exclude=FALSE)
{
  res = gh("GET /orgs/:org/members", org=org, .token=get_github_token(), .limit=get_api_limit()) %>%
    map_chr("login")

  if (!is.null(filter))
  {
    subset = res %>% str_detect(filter)

    if (exclude)
      res = res[!subset]
    else
      res = res[subset]
  }

  res
}

get_org_teams = function(org, filter=NULL, exclude=FALSE)
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

create_org_teams = function(org, teams=character(), privacy = c("closed","secret"),
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

add_org_team_member = function(org, users, teams, create_missing_teams=FALSE, verbose=TRUE, delay=0.2)
{
  stopifnot(!missing(org))
  stopifnot(is.character(users) & length(users) >=1)
  stopifnot(is.character(teams) & length(teams) >=1)
  stopifnot(length(users)==length(teams) | length(teams) == 1)

  info = data.frame(users, teams)

  team_ids = get_org_teams(org)

  new_teams = setdiff(unique(teams), names(team_ids))

  if (length(new_teams) != 0)
  {
    if (create_missing_teams)
      create_org_teams(org=org, new_teams, verbose=verbose)
    else
      stop("Team(s) ",paste(new_teams,collapse=", "), " do(es) not exist for ", org)
  }

  for(i in seq_along(nrow(info)))
  {
    team = info$teams[i]
    acc  = info$users[i]
    team_id = team_ids[team]

    if (verbose)
      cat("Adding ", acc, " to ", team, "...\n", sep="")

    gh("PUT /teams/:id/memberships/:username",
       id=id, username=acc,
       role="member",
       .token=token)

    Sys.sleep(delay)
  }
}
