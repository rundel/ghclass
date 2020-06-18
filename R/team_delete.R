github_api_team_delete = function(team_id) {
  gh::gh("DELETE /teams/:team_id",
         team_id = team_id,
         .token = github_get_token())
}

#' Delete team
#'
#' `team_delete` deletes an existing team from a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param team Character. Name of the GitHub team within that organization.
#' @param prompt Logical. Should the user be prompted before deleting repositories. Default `true`.
#'
#' @export
#'
team_delete = function(org, team, prompt = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(team, allow_null = TRUE)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = cli_yeah("This command will delete the following teams permanently: {.val {team}}.")
    if (!delete) {
      return(invisible())
    }
  }

  team = team_id_lookup(team, org)

  purrr::pwalk(
    team,
    function(team, id) {

      if (is.na(id)) {
        cli::cli_alert_danger("Team {.val {team}} does not exist in org {.val {org}}.")
        return()
      }

      res = purrr::safely(github_api_team_delete)(id)

      status_msg(
        res,
        "Deleted team {.val {team}} from org {.val {org}}.",
        "Failed to delete team {.val {team}} from org {.val {org}}."
      )
    }
  )
}
