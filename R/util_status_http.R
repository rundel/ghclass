# curl calls this roughly once a second while a request is idle and more
# often while data moves; it must never error and must return a logical.
status_curl_callback = function(down, up) {
  tryCatch(status_redraw(), error = function(e) NULL)
  TRUE
}

# httr2 has no global hook for curl options, so requests made while a
# display is active go through its mock hook and are performed with the
# progress callback attached.
status_httr2_options = function(destfile = NULL) {
  # The mock hook cannot see req_perform()'s download path. Existing mocks
  # must also take precedence over our hook, which performs real requests.
  if (!is.null(destfile) || !is.null(getOption("httr2_mock")) || is.null(status_display_scope()))
    return(list())

  list(httr2_mock = function(req) {
    req = httr2::req_options(req, noprogress = FALSE, xferinfofunction = status_curl_callback)
    httr2::req_perform(req, mock = NULL)
  })
}

status_httr_config = function() {
  if (is.null(status_display_scope()))
    return(httr::config())

  httr::config(noprogress = FALSE, progressfunction = status_curl_callback)
}
