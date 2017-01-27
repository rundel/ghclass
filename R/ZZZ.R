.ghclass = new.env(FALSE, parent=globalenv())

.onAttach = function(libname, pkgname)
{
  assign("api_limit", value = 1000L, envir = .ghclass)
  assign("phantom", value = NULL, envir = .ghclass)
  assign("session", value = NULL, envir = .ghclass)
  assign("token",   value = NULL, envir = .ghclass)
  assign("wercker", value = NULL, envir = .ghclass)

  try(get_github_token(),silent = TRUE)
  try(get_wercker_account(),silent = TRUE)
}

.onUnload = function(libpath)
{
  stop_session()
  stop_phantom()
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
