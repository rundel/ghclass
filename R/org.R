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

create_org_teams = function(org, teamnames) # FIXME
{
  # org = args[1]
  # account_file = args[2]
  # account_col = args[3]
  # team_col = args[4]
  #
  # stopifnot(file.exists(account_file))
  # team_info = read.csv(account_file, stringsAsFactors=FALSE)
  #
  # stopifnot(all(c(account_col, team_col) %in% names(team_info)))
  # stopifnot(all(team_info[[team_col]] != ""))
  #
  # teams = team_info[[team_col]] %>%
  #   unique() %>%
  #   sort()
  #
  #
  # for(team in teams)
  # {
  #   Sys.sleep(0.2)
  #
  #   cat("Adding ", team, "...\n", sep="")
  #   gh("POST /orgs/:org/teams",
  #      org=org,
  #      name=team, privacy="closed",
  #      .token=token)
  # }
  #
  # teams = gh("/orgs/:org/teams", org=org, .token=token)
  # team_ids = sapply(teams, function(x) x$id)
  # names(team_ids) = sapply(teams, function(x) x$name)
  #
  # for(i in seq_len(nrow(team_info)))
  # {
  #   Sys.sleep(0.2)
  #
  #   team = team_info[[team_col]][i]
  #   acc = team_info[[account_col]][i]
  #   id = team_ids[team]
  #
  #   if (acc == "")
  #     next
  #
  #   cat("Adding ", acc, " to ", team, "...\n", sep="")
  #
  #   gh("PUT /teams/:id/memberships/:username",
  #      id=id, username=acc,
  #      role="member",
  #      .token=token)
  # }
}
