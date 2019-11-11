github_get_api_limit = function() {
  getOption("ghclass.api.limit", 5000L)
}

github_set_api_limit = function(limit) {
  options("ghclass.api.limit" = limit)
}
