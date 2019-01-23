wercker_api_add_env_var = function(repo, key, value, protected=TRUE) {
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

wercker_api_get_env_var = function(repo, id)
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

#' @export
add_wercker_env_var = function(repo, key, value, protected=FALSE)
{
  id = purrr::pmap_chr(
    list(repo, key, value, protected),
    wercker_api_add_env_var
  )

  invisible(id)
}

#' @export
get_wercker_env_vars = function(repo)
{
  require_valid_repo(repo)

  purrr::map2_df(
    repo, get_wercker_app_id(repo),
    wercker_api_get_env_var
  )
}
