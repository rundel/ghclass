add_wercker_build_pipeline = function(app_id)
{
  req = httr::POST(
    "https://app.wercker.com/api/v3/pipelines",
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      application = app_id,
      name = "build",
      permissions = "public",
      pipelineName = "build",
      setScmProviderStatus = TRUE,
      type = "git"
    )
  )
  httr::stop_for_status(req)

  res = httr::content(req)
  stopifnot(!is.null(res$id))

  res$id
}

add_wercker_app = function(repo, org_id, privacy = c("public", "private"), provider = "github")
{
  privacy = match.arg(privacy)

  req = httr::POST(
    "https://app.wercker.com/api/v2/applications",
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      owner = org_id,
      privacy = privacy,
      scmName = get_repo_name(repo),
      scmOwner = get_repo_owner(repo),
      scmProvider = provider,
      stack = "6" # Docker
    )
  )
  httr::stop_for_status(req)

  res = httr::content(req)
  stopifnot(!is.null(res$success))

  app_id = res$data$id

  is_private = gh("GET /repos/:owner/:repo",
                  owner=get_repo_owner(repo),
                  repo = get_repo_name(repo),
                  .token = get_github_token())$private

  if (is_private) # setup a deploy key for private repos
  {
    add_wercker_deploy_key(repo, app_id)
  }

  pipeline_id = add_wercker_build_pipeline(app_id)
  run_id = run_wercker_pipeline(pipeline_id)

  invisible(NULL)
}

run_wercker_pipeline = function(pipeline_id)
{
  req = httr::POST(
    "https://app.wercker.com/api/v3/runs",
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      pipelineId = pipeline_id
    )
  )
  httr::stop_for_status(req)
  res = httr::content(req)
  stopifnot(!is.null(res$id))

  res$id
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
  httr::stop_for_status(req)
  httr::content(req)
}

add_key_to_github = function(repo, key_id, provider = "github")
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
  httr::stop_for_status(req)
  res = httr::content(req)
  stopifnot(!is.null(res$success))
}

add_key_to_app = function(app_id, key_id, type="unique")
{
  req = httr::POST(
    paste0("https://app.wercker.com/api/v2/applications/",app_id,"/checkoutKey"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      checkoutKeyId = key_id,
      checkoutKeyType = type
    )
  )
  httr::stop_for_status(req)
  res = httr::content(req)
  stopifnot(!is.null(res$success))
}

add_wercker_deploy_key = function(repo, app_id, provider = "github")
{
  key = get_wercker_deploy_key()
  add_key_to_github(repo, key$id, provider)
  add_key_to_app(app_id, key$id)
}

#' @export
get_wercker_whoami = function()
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v2/profile"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )
  httr::stop_for_status(req)
  httr::content(req, as="text") %>%
    jsonlite::fromJSON()
}

#' @export
get_wercker_orgs = function()
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v2/users/me/organizations"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )
  httr::stop_for_status(req)
  httr::content(req, as="text") %>%
    jsonlite::fromJSON() %>%
    select(-allowedActions)
}

#' @export
get_wercker_apps = function(owner)
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v3/applications/", owner, "?limit=100"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )
  httr::stop_for_status(req)
  httr::content(req, as="text") %>% jsonlite::fromJSON()
}

get_wercker_org_id = function(org)
{
  orgs = get_wercker_orgs() %>%
    filter(username == org)

  if (nrow(orgs) != 1)
    stop("Unable to find organization called ", wercker_org, " on wercker.")

  orgs$id
}

get_wercker_app_id = function(repo)
{
  app = purrr::map(repo, get_wercker_app_info)
  purrr::map_chr(app, "id")
}


get_wercker_app_info = function(repo)
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v3/applications/", repo),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )
  httr::stop_for_status(req)
  httr::content(req)
}


get_wercker_badge_key = function(repos)
{
  app_info = map(repos, get_wercker_app_info)
  data_frame(repo = repos, badge_key = purrr::map_chr(app_info, "badgeKey"))
}

#' @export
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

#' @export
add_wercker = function(repos, wercker_org, add_badges=TRUE, verbose=TRUE)
{
  require_valid_repo(repos)
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
    if (add_badges)
      add_wercker_badges(repo)
  }
}

#' @export
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
         .token=get_github_token(), .limit=get_github_api_limit())
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
          cat("Added badge to ", repo, " ...\n", sep="")
      },
      error = function(e)
        message("Adding badge to ", repo, " failed.")
    )
  }
}



#' @export
add_wercker_env_var = function(repo, key, value, protected=FALSE)
{
  id = purrr::pmap_chr(
    list(repo, key, value, protected),
    function(repo, key, value, protected=TRUE) {
      req = httr::POST(
        "https://app.wercker.com/api/v3/envvars",
        httr::add_headers(
          Authorization = paste("Bearer", get_wercker_token())
        ),
        encode = "json",
        body = list(
          key       = as.character(key),
          protected = protected,
          scope     = "application",
          target    = get_wercker_app_id(repo),
          value     = as.character(value)
        )
      )

      httr::stop_for_status(req)

      purrr::pluck(httr::content(req), "id", .default = NA)
    }
  )

  invisible(id)
}


#' @export
get_wercker_env_vars = function(repo)
{
  require_valid_repo(repo)

  purrr::map2_df(
    repo, get_wercker_app_id(repo),
    function(repo, id)
    {
      req = httr::GET(
        paste0("https://app.wercker.com/api/v3/envvars?scope=application&target=", id),
        httr::add_headers(
          Authorization = paste("Bearer", get_wercker_token())
        ),
        encode = "json"
      )
      httr::stop_for_status(req)

      c(
        repo = repo,
        repo_id = id,
        purrr::pluck(httr::content(req), "results", 1L)
      )
    }
  )
}
