# Mirrors gh's handling of GITHUB_API_URL so URLs built outside gh::gh()
# (httr calls, clone/push targets, badge links) point at the same instance.

github_api_env_url = function() {
  url = Sys.getenv("GITHUB_API_URL", unset = "https://api.github.com")
  if (!grepl("^https?://[^/]+", url))
    cli_stop("{.envvar GITHUB_API_URL} must be an http(s) URL, not {.val {url}}.")
  url
}

github_host_url = function() {
  base = sub("^(https?://[^/]+).*$", "\\1", github_api_env_url())
  sub("api\\.github\\.com$", "github.com", base)
}

github_api_url = function() {
  host = github_host_url()
  if (grepl("^https?://github\\.com$", host))
    sub("github\\.com$", "api.github.com", host)
  else
    paste0(host, "/api/v3")
}

github_graphql_url = function() {
  host = github_host_url()
  if (grepl("^https?://github\\.com$", host))
    paste0(github_api_url(), "/graphql")
  else
    paste0(host, "/api/graphql")
}
