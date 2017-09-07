add_wercker_app = function(repo, org_id, privacy = c("public", "private"), provider = "github")
{
  is_private = gh("GET /repos/:owner/:repo",
                  owner=get_repo_owner(repo),
                  repo = get_repo_name(repo),
                  .token = get_github_token())$private

  if (is_private) # setup a deploy key for private repos
  {
    res = add_wercker_deploy_key(repo)
    stopifnot(!is.null(res$success))
  }

  privacy = match.arg(privacy)

  req = POST(
    "https://app.wercker.com/api/v2/applications",
    add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      owner = org_id,
      privacy = privacy,
      scmName = get_repo_name(repo),
      scmOwner = get_repo_owner(repo),
      scmProvider = provider,
      stack = "6"
    )
  )

  res = httr::content(req)
  stopifnot(!is.null(res$success))

  res
}

get_wercker_deploy_key = function()
{
  req = httr::POST(
    paste0("https://app.wercker.com/api/v2/checkoutKeys"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )

  httr::content(req)
}

add_wercker_deploy_key = function(repo, key_id = get_wercker_deploy_key()$id, provider = "github")
{
  req = httr::POST(
    paste0("https://app.wercker.com/api/v2/checkoutKeys/",key_id,"/link"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      scmName = get_repo_name(repo),
      scmOwner = get_repo_owner(repo),
      scmProvider = provider
    )
  )

  httr::content(req)
}


get_wercker_whoami = function()
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v2/profile"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )

  content(req, as="text") %>%
    jsonlite::fromJSON()
}

get_wercker_orgs = function()
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v2/users/me/organizations"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )

  content(req, as="text") %>%
    jsonlite::fromJSON() %>%
    select(-allowedActions)
}

get_wercker_apps = function(user)
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v3/applications/", user),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    body = list(limit = 100),
    encode = "json"
  )

  content(req, as="text") %>% jsonlite::fromJSON()
}

get_wercker_org_id = function(org)
{
  orgs = get_wercker_orgs() %>%
    filter(username == org)

  if (nrow(orgs) != 1)
    stop("Unable to find organization called ", wercker_org, " on wercker.")

  orgs$id
}

add_wercker = function(repos, wercker_org, verbose=TRUE)
{
  require_valid_repo(repos, require_owner = TRUE)
  org_id = get_wercker_org_id(wercker_org)

  for(repo in repos)
  {
    if (verbose)
      cat("Creating wercker app for", repo, "...\n")

    add_wercker_app(repo, org_id)
  }
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
      Sys.sleep(2)
      click_element(paste0("input#", size))
      click_element(paste0("input#", branch))
      click_element(paste0("input#", type))
      get_element(".sharingBadge textarea")
    }
  )
}


set_wercker_env = function(repos, key, value, protected = TRUE, verbose=TRUE, debug=FALSE)
{
  stopifnot(all(valid_repo(repos, require_owner = TRUE)))

  url = wercker_app_url(repos) %>% paste0("/environment")

  wercker_login(debug=debug)
  session = get_session()
  session$takeScreenshot()

  pmap(
    data_frame(url, key, value, protected),
    function(url, key, value)
    {
      session$go(url)
      set_element('td.envvarItem_key input', key)
      set_element("textarea.false", value)
      if (protected)
        click_element('input[type="checkbox"]')

      click_element(button[class="small radius"])

      session$takeScreenshot()
      Sys.sleep(2)

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

