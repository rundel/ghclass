#' Use Temporary GitHub Access Token
#'
#' Temporarily change the `GITHUB_PAT` environmental variable for
#' GitHub authentication. Based on the `withr` interface.
#'
#' @param new Temporary GitHub access token
#' @param code Code to execute with the temporary token
#' @param .local_envir The environment to use for scoping.
#'
#' @details if `NA` is used the `GITHUB_PAT` environment variable will be unset.
#'
#' @family authentication
#'
#' @export
#'
with_pat = function(new, code) {
  arg_is_chr_scalar(new)
  withr::with_envvar(c("GITHUB_PAT" = new), code, action = "replace")
}

#' @rdname with_pat
#' @export
#'
local_pat = function(new, .local_envir = parent.frame()) {
  arg_is_chr_scalar(new)
  withr::local_envvar(c("GITHUB_PAT" = new), action = "replace",
                      .local_envir = .local_envir)
}



