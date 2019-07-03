github_api_org_accept_invite = function(org, token) {
  arg_is_chr_scalar(org, token)

  gh::gh(
    "PATCH /user/memberships/orgs/:org",
    org = org,
    state = "active",
    .token = token
  )
}

org_accept_invite = function(org, user, pat) {
  arg_is_chr(org, pat)

  purrr::pwalk(
    list(org, user, pat),
    function(org, user, pat) {
      res = purrr::safely(github_api_org_accept_invite)(org, pat)

      status_msg(
        res,
        glue::glue("Accepted {usethis::ui_value(user)}s invite to org {usethis::ui_value(org)}."),
        glue::glue("Failed to accept {usethis::ui_value(user)}s invite to org {usethis::ui_value(org)}.")
      )
    }
  )
}
