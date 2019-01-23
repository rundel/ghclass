get_wercker_badge_key = function(repo)
{
  app_info = purrr::map(repo, wercker_api_get_app)
  purrr::map_chr(app_info, "badgeKey", .default=NA)
}

#' @export
get_wercker_badge = function(repo, size = "small", type = "markdown", branch = "master")
{
  size = match.arg(size, c("small", "large"), several.ok = TRUE)
  type = match.arg(type, c("markdown", "html"), several.ok = TRUE)

  size = switch(
    size,
    small = "s",
    large = "m"
  )

  key = get_wercker_badge_key(repo)

  purrr::pmap_chr(
    list(size, type, key, branch),
    function(size, type, key, branch) {
      img_url = sprintf("https://app.wercker.com/status/%s/%s/%s", key, size, branch)
      app_url = sprintf("https://app.wercker.com/project/byKey/%s", key)

      if (type == "markdown")
        sprintf('[![wercker status](%s "wercker status")](%s)', img_url, app_url)
      else
        sprintf('<a href="%s"><img alt="Wercker status" src="%s"></a>', app_url, img_url)
    }
  )
}


strip_existing_badge = function(content)
{
  md_badge_pattern = "\\[\\!\\[wercker status\\]\\(.*? \"wercker status\"\\)\\]\\(.*?\\)\n*"
  html_badge_pattern = "<a href=\".*?\"><img alt=\"Wercker status\" src=\".*?\"></a>"

  content = gsub(md_badge_pattern, "", content)
  content = gsub(html_badge_pattern, "", content)
}





#' @export
add_wercker_badge = function(repo, badge = get_wercker_badge(repo, branch = branch),
                             branch = "master", strip_existing_badge = TRUE, verbose = TRUE)
{
  require_valid_repo(repo)

  res = purrr::pmap(
    list(repo, badge, branch),
    function(repo, badge, branch) {

      if (verbose)
        message("Adding wercker badge to ", repo, " ...")

      readme = get_readme(repo, branch)

      if (is.null(readme)) { # README.md does not exist
        content = paste0(badge,"\n\n")
        gh_file = "README.md"
      } else {
        cur_readme = rawToChar(base64enc::base64decode(readme$content))
        if (strip_existing_badge)
          cur_readme = strip_existing_badge(cur_readme)

        gh_file = purrr::pluck(readme,"path")
        content = paste0(badge, "\n\n", cur_readme)
      }

      put_file(repo, file=gh_file, content=charToRaw(content),
               message="Added wercker badge", branch=branch)
    }
  )

  purrr::walk2(
    repo[check_errors(res)], get_errors(res),
    function(repo, error) {
      msg = sprintf("Adding badge to %s failed.\n", repo)
      if (verbose) {
        bad_repos = paste0(repo, ": ", error)
        msg = paste0(msg, format_list(bad_repos))
      }

      warning(msg, call. = FALSE, immediate. = TRUE)
    }
  )
}
