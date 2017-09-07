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
    paste0("https://app.wercker.com/api/v3/applications/", user,"?limit=100"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
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

  existing_apps = get_wercker_apps(wercker_org)

  for(repo in repos)
  {
    if (get_repo_name(repo) %in% existing_apps$name)
    {
      if (verbose)
        cat("Skipping, app already exists for", repo, "...\n")
      next
    }

    if (verbose)
      cat("Creating wercker app for", repo, "...\n")

    add_wercker_app(repo, org_id)
  }
}

get_wercker_app_info = function(repos)
{
  map(
    repos,
    function(repo)
    {
      req = httr::GET(
        paste0("https://app.wercker.com/api/v3/applications/", repo),
        httr::add_headers(
          Authorization = paste("Bearer", get_wercker_token())
        ),
        encode = "json"
      )

      content(req)
    }
  )
}

get_wercker_badge_key = function(repos)
{
  app_info = get_wercker_app_info(repos)
  data_frame(repo = repos, badge_key = map_chr(app_info, "badgeKey"))
}

get_wercker_badges = function(repos, size = c("small", "large"), branch = "master")
{
  size = match.arg(size)
  size_lookup = c("small" = "s", "large" = "m")

  get_wercker_badge_key(repos) %>%
    mutate(
      img_url = sprintf("https://app.wercker.com/status/%s/%s/%s", badge_key, size_lookup[size], branch),
      app_url = sprintf("https://app.wercker.com/project/byKey/%s", badge_key)
    ) %>%
    mutate(
      markdown_link = sprintf('[![wercker status](%s "wercker status")](%s)', img_url, app_url),
      html_link = sprintf('<a href="%s"><img alt="Wercker status" src="%s"></a>', app_url, img_url)
    )
}

add_wercker_badges = function(repos, badges=get_wercker_badges(repos)$markdown_link, branch = "master",
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

