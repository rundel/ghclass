# Classic token scopes and the scopes each one implies, from
# https://docs.github.com/en/apps/oauth-apps/building-oauth-apps/scopes-for-oauth-apps
github_scope_hierarchy = list(
  "repo" = c("repo:status", "repo_deployment", "public_repo", "repo:invite", "security_events"),
  "write:packages" = "read:packages",
  "admin:org" = c("write:org", "read:org", "manage_runners:org"),
  "write:org" = "read:org",
  "admin:public_key" = c("write:public_key", "read:public_key"),
  "write:public_key" = "read:public_key",
  "admin:repo_hook" = c("write:repo_hook", "read:repo_hook"),
  "write:repo_hook" = "read:repo_hook",
  "user" = c("read:user", "user:email", "user:follow"),
  "project" = "read:project",
  "write:discussion" = "read:discussion",
  "admin:enterprise" = c("manage_runners:enterprise", "manage_billing:enterprise", "read:enterprise"),
  "audit_log" = "read:audit_log",
  "codespace" = "codespace:secrets",
  "copilot" = "manage_billing:copilot",
  "admin:gpg_key" = c("write:gpg_key", "read:gpg_key"),
  "write:gpg_key" = "read:gpg_key",
  "admin:ssh_signing_key" = c("write:ssh_signing_key", "read:ssh_signing_key"),
  "write:ssh_signing_key" = "read:ssh_signing_key"
)

# Scopes used by ghclass, with the reason each one is needed
ghclass_scopes = c(
  "repo" = "needed by nearly all repo_*(), org_*(), and team_*() functions.",
  "admin:org" = "needed by org_sitrep(), org_invite(), org_set_*(), and the team_*() functions.",
  "workflow" = "needed to add or modify files under .github/workflows/.",
  "delete_repo" = "needed by repo_delete()."
)

parse_scopes = function(x) {
  if (is.null(x) || length(x) == 0)
    return(character())

  x = trimws(strsplit(paste(x, collapse = ","), ",")[[1]])
  x[nzchar(x)]
}

expand_scopes = function(scopes) {
  implied = unlist(github_scope_hierarchy[scopes], use.names = FALSE)
  unique(c(scopes, implied))
}

# Prefixes from https://github.blog/2021-04-05-behind-githubs-new-authentication-token-formats/
token_type = function(token) {
  prefixes = c(
    "github_pat_" = "fine-grained personal access token",
    "ghp_" = "classic personal access token",
    "gho_" = "OAuth access token",
    "ghu_" = "GitHub App user access token",
    "ghs_" = "GitHub App installation access token"
  )

  match = purrr::keep(names(prefixes), ~ startsWith(as.character(token), .x))

  if (length(match) == 0)
    "unknown"
  else
    unname(prefixes[match[1]])
}

token_source = function(token) {
  token = as.character(token)

  if (identical(token, Sys.getenv("GITHUB_PAT")))
    return("GITHUB_PAT environment variable")

  if (identical(token, Sys.getenv("GITHUB_TOKEN")))
    return("GITHUB_TOKEN environment variable")

  if (identical(token, as.character(gh::gh_token())))
    return("gitcreds")

  "supplied directly"
}

# GitHub reports the token's scopes and the scopes an endpoint accepts on every
# response. Tokens without classic scopes (fine-grained PATs, GitHub App tokens)
# omit x-oauth-scopes entirely, so there is nothing to compare for them.
missing_scope_hint = function(headers) {
  granted = headers[["x-oauth-scopes"]]
  accepted = parse_scopes(headers[["x-accepted-oauth-scopes"]])

  if (is.null(granted) || length(accepted) == 0)
    return(NULL)

  granted = parse_scopes(granted)

  if (any(accepted %in% expand_scopes(granted)))
    return(NULL)

  paste0(
    if (length(accepted) > 1) "one of " else "",
    paste(accepted, collapse = ", "),
    " (token has ", if (length(granted) == 0) "none" else paste(granted, collapse = ", "), ")"
  )
}
