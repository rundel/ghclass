.ghclass = new.env(FALSE, parent=globalenv())

.onAttach = function(libname, pkgname) {
  assign("api_limit", value = 1000L, envir = .ghclass)
  assign("github_token", value = NULL, envir = .ghclass)
  assign("wercker_token", value = NULL, envir = .ghclass)

  try(get_github_token(), silent = TRUE)
  try(get_wercker_token(), silent = TRUE)
}

.onUnload = function(libpath) {
}

#' @export
get_github_api_limit = function() {
  get("api_limit", envir = .ghclass)
}

#' @export
set_github_api_limit = function(limit) {
  limit = as.integer(limit)
  stopifnot(limit >= 1)

  assign("api_limit", value=limit, envir = .ghclass)
}
