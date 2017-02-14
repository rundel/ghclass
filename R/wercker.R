wercker_create_app = function(repo, wercker_org, verbose=TRUE, debug=FALSE)
{
  session = get_session()
  session$takeScreenshot()

  if (missing(wercker_org))
    wercker_org = get_repo_owner(repo)

  if (debug)
    cat("Creating wercker app for", repo, "in", werker_org, "\n")

  create_url = "https://app.wercker.com/applications/create"

  # Connect and wait for loading
  session$go(create_url)
  wait_for_element("li.js-repository.private")
  set_element(".js-repository-filter", repo)

  if (debug)
    cat("  * Connected and loaded repos\n")

  # Select repo
  repos = session$findElements(css="li.js-repository:not(.force-hidden)")
  repo_names = map_chr(repos, ~ .$getText()) %>%
    str_replace("\n.*","") %>%
    str_replace(" / ", "/")

  matches = which(repo == repo_names)

  if (length(matches) != 1) {
    if (debug)
      session$takeScreenshot()
    stop("Unable to find unique match for repo ", repo)
  }

  repos[[matches]]$click()
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
}

add_wercker = function(repos, wercker_org, verbose=TRUE, debug=FALSE)
{
  require_valid_repo(repos, require_owner = TRUE)

  wercker_login(debug=debug)
  for(repo in repos)
  {
    wercker_create_app(repo, wercker_org, verbose=verbose, debug=debug)
  }
}

wercker_repo_url = function(repos)
{
  stopifnot(all(valid_repo(repos, requier_owner = TRUE)))



  return(urls)
}



get_badges = function(repos, wercker_org)
{

}
