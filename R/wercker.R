wercker_api_checkout_key = function() {
  req = httr::POST(
    "https://app.wercker.com/api/v2/checkoutKeys",
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )
  httr::stop_for_status(req)

  httr::content(req)
}

wercker_api_link_key = function(repo, provider = "github", key) {
  repo_owner = get_repo_owner(repo)
  repo_name  = get_repo_name(repo)

  req = httr::POST(
    paste0("https://app.wercker.com/api/v2/checkoutKeys/",key[["id"]],"/link"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      scmName     = repo_name,
      scmOwner    = repo_owner,
      scmProvider = provider
    )
  )
  httr::stop_for_status(req)

  res = httr::content(req)
  stopifnot(res[["success"]])

  invisible(res)
}

wercker_api_add_app = function(repo, provider, privacy, wercker_org_id, key) {
  repo_owner = get_repo_owner(repo)
  repo_name  = get_repo_name(repo)

  req = purrr::safely(httr::POST)(
    paste0("https://app.wercker.com/api/v2/applications"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token()),
      origin = "https://app.wercker.com",
      referer = "https://app.wercker.com/applications/create"
    ),
    encode = "json",
    body = list(
      checkoutKeyId = key[["id"]],
      owner         = wercker_org_id,
      privacy       = privacy,
      pushKey       = "",
      scmName       = repo_name,
      scmOwner      = repo_owner,
      scmProvider   = provider,
      sshUrl        = "",
      stack         = "6"
    ),
    httr::timeout(120)
  )

  if (!is.null(req$error)) {
    warning("", req$error, immediate. = TRUE)
  }

  #res = httr::content(req)
  #stopifnot(res[["success"]])
  #invisible(res)

  invisible(NULL)
}

wercker_api_add_build_pipeline = function(app_id, privacy) {

  req = httr::POST(
    paste0("https://app.wercker.com/api/v3/pipelines"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      application = app_id,
      name = "build",
      permissions = privacy,
      pipelineName = "build",
      setScmProviderStatus = TRUE,
      type = "git"
    )
  )


  httr::stop_for_status(req)

  res = httr::content(req)
  invisible(res)
}

wercker_api_delete_app = function(repo) {

  id = get_wercker_app_id(repo)

  req = httr::POST(
    paste0("https://app.wercker.com/deleteproject"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json",
    body = list(
      id = id
    )
  )

  httr::stop_for_status(req)

  res = httr::content(req)
  stopifnot(res[["success"]])
  invisible(res)
}


wercker_api_get_pipelines = function(repo, as_df = TRUE)
{
  stopifnot(length(repo) == 1)
  require_valid_repo(repo)

  req = httr::GET(
    paste0("https://app.wercker.com/api/v3/applications/", repo, "/pipelines?limit=60"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )

  httr::stop_for_status(req)

  if (as_df)
    jsonlite::fromJSON( httr::content(req, as="text") )
  else
    httr::content(req)
}

wercker_api_get_profile = function()
{
  req = httr::GET(
    paste0("https://app.wercker.com/api/v2/profile"),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )
  httr::stop_for_status(req)
  httr::content(req)
}

wercker_api_get_app = function(repo, strict = FALSE)
{
  stopifnot(length(repo) == 1)
  require_valid_repo(repo)

  req = httr::GET(
    paste0("https://app.wercker.com/api/v3/applications/", repo),
    httr::add_headers(
      Authorization = paste("Bearer", get_wercker_token())
    ),
    encode = "json"
  )

  if (strict)
    httr::stop_for_status(req)

  if (httr::status_code(req) < 300) {
    httr::content(req)
  } else {
    NULL
  }
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

  res = httr::content(req, as="text")
  res = jsonlite::fromJSON(res)
  res[-which(names(res) == "allowedActions")]
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
  res = httr::content(req, as="text")
  jsonlite::fromJSON(res)
}

get_wercker_org_id = function(org)
{
  orgs = get_wercker_orgs()
  orgs = orgs[orgs$username == org,]

  if (nrow(orgs) != 1)
    stop("Unable to find organization called ", org, " on wercker.", call. = FALSE)

  orgs[["id"]]
}


get_wercker_app_id = function(repo)
{
  app = purrr::map(repo, wercker_api_get_app)
  purrr::map_chr(app, "id", .default=NA)
}

wercker_pipelines_exist = function(repo)
{
  p = purrr::map(repo, wercker_api_get_pipelines, as_df=FALSE)
  p = purrr::map_int(p, length)
  p != 0
}

wercker_app_exists = function(repo)
{
  p = purrr::map(repo, wercker_api_get_app)
  purrr::map_lgl(p, ~!is.null(.x))
}



add_wercker_app = function(repo, wercker_org = get_repo_owner(repo), privacy = c("public", "private"), provider = "github")
{
  privacy = match.arg(privacy)
  org_id = get_wercker_org_id(wercker_org)

  key = wercker_api_checkout_key()
  wercker_api_link_key(repo, provider, key)

  wercker_api_add_app(repo, provider, privacy, org_id, key)
  wercker_api_add_build_pipeline(wercker_api_get_app(repo, strict = TRUE)$id, privacy)

  invisible(NULL)
}

#' @export
check_wercker = function(repo)
{
  apps = wercker_app_exists(repo)
  pipes = rep(FALSE, length(repo))
  pipes[apps] = wercker_pipelines_exist(repo[apps])

  tibble::data_frame(
    repo = repo,
    app_exists = apps,
    pipelines_exists = pipes
  )
}


#' @export
add_wercker = function(repo, wercker_org = get_repo_owner(repo), add_badge=TRUE, verbose=TRUE)
{
  require_valid_repo(repo)

  purrr::walk2(
    repo, wercker_org,
    function(repo, wercker_org) {

      existing_apps = get_wercker_apps(wercker_org)[["name"]]

      if (get_repo_name(repo) %in% existing_apps) {
        if (verbose)
          message("Skipping, app already exists for ", repo, " ...")
        return()
      }

      if (verbose)
        message("Creating wercker app for ", repo, " ...")

      res = purrr::safely(add_wercker_app)(repo, wercker_org)

      if (!is.null(res$error)) {
        message("App creation failed:", res$error)
        if (wercker_app_exists(repo))
          wercker_api_delete_app(repo)
      } else {
        if (add_badge)
          add_wercker_badge(repo)
      }
    }
  )
}



