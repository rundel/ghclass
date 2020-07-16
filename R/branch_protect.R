# github_api_branch_protect = function(repo, branch) {
#   ghclass_api_v3_req(
#     "PUT /repos/:owner/:repo/branches/:branch/protection",
#     owner = get_repo_owner(repo),
#     repo = get_repo_name(repo),
#     branch = branch,
#     required_status_checks = NA,
#     enforce_admins = NA,
#     required_pull_request_reviews = NA,
#     restrictions = list(
#       users = list(),
#       teams = list()
#     )
#   )
# }
#
# github_api_branch_unprotect = function(repo, branch) {
#   ghclass_api_v3_req(
#     "DELETE /repos/:owner/:repo/branches/:branch/protection",
#     owner = get_repo_owner(repo),
#     repo = get_repo_name(repo),
#     branch = branch
#   )
# }
#
#
#
# branch_protect = function(repo, branch = "master") {
#   arg_is_chr(repo, branch)
#
#   purrr::walk2(
#     repo, branch,
#     function(repo, branch) {
#       res = purrr::safely(github_api_branch_protect)(repo, branch)
#
#       repo_fmt = format_repo(repo, branch)
#
#       status_msg(
#         res,
#         "Protecting branch {.val {repo_fmt}}.",
#         "Failed to protect branch {.val {repo_fmt}}."
#       )
#     }
#   )
# }
#
# branch_unprotect = function(repo, branch = "master") {
#   arg_is_chr(repo, branch)
#
#   purrr::walk2(
#     repo, branch,
#     function(repo, branch) {
#
#       res = purrr::safely(github_api_branch_unprotect)(repo, branch)
#
#       repo_fmt = format_repo(repo, branch)
#
#       status_msg(
#         res,
#         "Removing protection from branch {.val {repo_fmt}}.",
#         "Failed to remove protection from branch {.val {repo_fmt}}."
#       )
#     }
#   )
# }
#
