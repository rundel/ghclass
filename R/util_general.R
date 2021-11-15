
replace_nas = function(cur, rep) {
  cur[is.na(cur)] = rep[is.na(cur)]
  cur
}

warn_experimental = function() {
  calling_func = as.character(sys.calls()[[sys.nframe()-1]])[1]

  cli::cli_warn(
    c( "i" = paste0(
      "The function {.fun {calling_func}} is currently ",
      "considered experimental. Its interface, implementation, and other ",
      "features may change significantly in future versions of the package. ",
      "Use with caution."
    ) ),
    .frequency = "once", .frequency_id = paste0("warn_experimental_", calling_func)
  )
}

url_encode = function(urls) {
  purrr::map_chr(urls, utils::URLencode)
}









