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

invite_users = function(org, users, verbose=TRUE)
{
  users %<>% clean_usernames() %>% tolower()
  current_members = get_org_members(org) %>% tolower()

  need_invite = setdiff(tolower(users), tolower(current_members))

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
