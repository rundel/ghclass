
replace_nas = function(cur, rep) {
  cur[is.na(cur)] = rep[is.na(cur)]
  cur
}

flag_experimental = function() {
  calling_func = as.character(sys.calls()[[sys.nframe()-1]])[1]

  cli_warn(
    "This function ({.val {calling_func}}) is currently ",
    "considered experimental. Its interface, implementation, and other ",
    "features may change significantly in future versions of the package. ",
    "Use with caution."
  )
}

url_encode = function(urls) {
  purrr::map_chr(urls, utils::URLencode)
}









