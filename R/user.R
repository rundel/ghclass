#' @name user
#' @rdname user
#'
#' @title GitHub user related tools
#'
#' @description
#'
#' * `user_exists` - returns `TRUE` if the username(s) (or organization) exist on GitHub
#' and `FALSE` otherwise. Note that GitHub considers organizations to be a type of user.
#'
#' * `user_repos` - returns a (filtered) vector of repositories belonging to the user.
#'
#' * `user_type` - returns a vector of the accounts' types.
#'
#' @param user Character. GitHub username(s).
#' @param type Character. Can be one of "all", "owner", "public", "private", "member".
#' @param filter Character. Regular expression pattern for matching (or excluding) repositories.
#' @param exclude Logical. Should entries matching the regular expression in `filter` be excluded or included?
#' @param full_repo Logical. Should the full repository address be returned
#' (e.g. `owner/repo` instead of just `repo`)?
#'
#'
#' @examples
#' \dontrun{
#' user_exists(c("rundel", "ghclass-test", "hopefullydoesnotexist"))
#'
#' user_repos("rundel", type = "public", filter = "ghclass")
#'
#' user_repos("ghclass-test")
#'
#' org_repos("ghclass-test")
#'
#' user_type(c("rundel", "ghclass-test"))
#' }

NULL

