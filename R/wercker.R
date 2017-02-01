
wercker_login = function(username, password)
{
  session = get_session()
  session$go("https://app.wercker.com/sessions/new/")
  set_element("#username", username)
  set_element("#password", password)
  click_element("#login")
}

wecker_create_app = function(org, repo, wercker_org = org, debug=FALSE)
{
  session = get_session()

  full_repo = paste0(org,"/",repo)

  if (debug)
    cat("Creating wercker app for", repo, "in", org, "\n")

  create_url = "https://app.wercker.com/applications/create"

  # Connect and wait for loading
  session$go(create_url)
  wait_for_element("li.js-repository.private")
  set_element(".js-repository-filter", full_repo)

  if (debug)
    cat("  * Connected and loaded repos\n")

  # Select repo
  repos = session$findElements(css="li.js-repository:not(.force-hidden)")

  if (length(repos) == 0) {
    stop("Unable to find repo ", full_repo)
  } else if (length(repos) > 1) {
    stop("Multiple repos matched ", full_repo)
  }

  repos[[1]]$click()
  click_element(".js-repository-selector-select")
  Sys.sleep(1)

  if (debug)
    cat("  * Selected repo\n")

  # Select org
  orgs = session$findElements(css=".js-owner-option")
  org_names = map_chr(orgs, ~ .x$getText())
  i = which(org_names == wercker_org)

  if (length(i) == 0) {
    stop("Unable to find wercker organization ", wercker_org)
  } else if (length(i) > 1) {
    stop("Multiple organizations matched ", wercker_org)
  }

  radio = orgs[[i]]$findElement(css = "input")
  radio$click()
  click_element(".js-select-owner-select")
  Sys.sleep(1)

  if (debug)
    cat("  * Selected organization\n")

  # Configure access (just use the default)
  click_element(".js-werckerbot-select")
  Sys.sleep(1)

  # Make app public
  click_element("#appIsPublic")
  Sys.sleep(1)

  # Create app
  click_element(".js-create-application")


  # Wait for creation to finish
  repeat {
    Sys.sleep(1)

    if (session$getUrl() != create_url)
      break
  }

  if (debug)
    cat("  * App created.\n\n")

  return(session$getUrl())
}

add_wercker = function(org, pattern)
{
  repos = get_org_repos(org, pattern)
  wercker_login(username, password)

  urls = list()

  for(repo in repos)
  {
    urls[[repo]] = wecker_create_app(org, repo, debug=TRUE)
  }
}


#wercker_login(session, username, password)
#session$takeScreenshot()
#wecker_create_app(session, repo, org, TRUE)


