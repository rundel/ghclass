wercker_create_app = function(repo, wercker_org = get_repo_owner(repo), verbose=TRUE, debug=FALSE)
{
  session = get_session()
  session$takeScreenshot()

  if (debug)
    cat("Creating wercker app for", repo, "in", wercker_org, "\n")

  create_url = "https://app.wercker.com/applications/create"

  # Connect and wait for loading
  session$go(create_url)
  wait_for_element("li.js-repository.private", timeout=120000)
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
    if (verbose)
      cat("Creating wercker app for", repo, "...\n")
    wercker_create_app(repo, wercker_org, verbose=verbose, debug=debug)
  }
}

wercker_app_url = function(apps)
{
  stopifnot(all(valid_repo(apps, require_owner = TRUE)))
  paste0("https://app.wercker.com/", apps)
}

require_valid_app = function(apps, require_owner=TRUE)
{
  valid = valid_repo(apps, require_owner = require_owner)
  if (!all(valid))
    stop("Invalid app names: \n\t", paste(apps[!valid], collapse="\n\t"))
}

get_badges = function(apps, size = c("small", "large"), branch=c("master","all"),
                      type=c("markdown","hmtl"), debug=FALSE)
{
  option_urls = wercker_app_url(apps) %>% paste0("/options")

  size   = match.arg(size)
  branch = match.arg(branch)
  type   = match.arg(type)

  wercker_login(debug=debug)

  session = get_session()
  session$takeScreenshot()

  map_chr(
    option_urls,
    function(url)
    {
      session$go(url)
      Sys.sleep(1)
      click_element(paste0("input#", size))
      click_element(paste0("input#", branch))
      click_element(paste0("input#", type))
      get_element(".sharingBadge textarea")
    }
  )
}

add_badges = function(repos, badges=get_badges(repos), branch = "master",
                      message = "Adding wercker badge", verbose=TRUE)
{
  stopifnot(all(valid_repo(repos, require_owner = TRUE)))

  repo_name  = get_repo_name(repos)
  repo_owner = get_repo_owner(repos)

  for(i in seq_along(repos))
  {
    repo = repo_name[i]
    owner = repo_owner[i]
    file = "README.md"

    readme = try({
      gh("GET /repos/:owner/:repo/readme",
         owner = owner, repo = repo, ref = branch,
         .token=get_github_token(), .limit=get_api_limit())
    }, silent = TRUE)

    tryCatch(
      {
        if (inherits(readme, "try-error")) # README.md does not exist
        {
          file = "README.md"
          content = paste0(badges[i],"\n\n") %>% charToRaw() %>% base64enc::base64encode()

          gh("PUT /repos/:owner/:repo/contents/:path",
             owner = owner, repo = repo, path=file,
             message = message, content = content,
             branch = branch, .token=get_github_token())
        } else {                           # README.md exists
          new_readme = base64enc::base64decode(readme$content) %>% rawToChar()
          file = readme$path
          content = paste0(badges[i],"\n\n", new_readme) %>% charToRaw() %>% base64enc::base64encode()

          cat(repo, owner, file, readme$sha,"\n", sep=" - ")
          gh("PUT /repos/:owner/:repo/contents/:path",
             owner = owner, repo = repo, path=file,
             message = message, content = content,
             sha = readme$sha, branch = branch,
             .token=get_github_token())
        }

        if (verbose)
          cat("Added badge to ", repo, ".\n", sep="")
      },
      error = function(e)
        message("Adding badge to ", repo, " failed.")
    )
  }
}

