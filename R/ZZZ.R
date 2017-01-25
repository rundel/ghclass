.ghclass = new.env(FALSE, parent=globalenv())

.onLoad = function(libname, pkgname) {
  assign("api_limit", value = 1000L, envir = .ghclass)
  get_github_token(quiet=TRUE)
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
