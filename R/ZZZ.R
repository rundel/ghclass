.ghclass = new.env(FALSE, parent=globalenv())

.onAttach = function(libname, pkgname) {
  assign("api_limit", value = 1000L, envir = .ghclass)
  assign("phantom", value = NULL, envir = .ghclass)
  assign("session", value = NULL, envir = .ghclass)
  get_github_token(quiet=TRUE)
}

.onUnload = function(libpath) {
  # Cleanup phantom
}

get_api_limit = function()
{
  get("api_limit", envir = .ghclass)
}

set_api_limit = function(limit)
{
  limit = as.integer(limit)
  stopifnot(limit >= 1)

  assign("api_limit", value=limit, envir = .ghclass)
}
