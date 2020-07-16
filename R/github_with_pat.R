#' @name github_with_pat
#' @rdname github_with_pat
#'
#' @title `withr`-like functions for temporary personal access token
#'
#' @description
#' Temporarily change the `GITHUB_PAT` environmental variable for
#' GitHub authentication. Based on the `withr` interface.
#'
#' @param new Temporary GitHub access token
#' @param code Code to execute with the temporary token
#' @param .local_envir The environment to use for scoping.
#'
#' @details if `new = NA` is used the `GITHUB_PAT` environment variable will be unset.
#'
NULL

#' @rdname github_with_pat
#' @export
#'
with_pat = function(new, code) {
  arg_is_chr_scalar(new)
  withr::with_envvar(c("GITHUB_PAT" = new), code, action = "replace")
}

#' @rdname github_with_pat
#' @export
#'
local_pat = function(new, .local_envir = parent.frame()) {
  arg_is_chr_scalar(new)
  withr::local_envvar(c("GITHUB_PAT" = new), action = "replace",
                      .local_envir = .local_envir)
}



