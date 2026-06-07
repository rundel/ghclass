
replace_nas = function(cur, rep) {
  cur[is.na(cur)] = rep[is.na(cur)]
  cur
}

warn_experimental = function(call = rlang::caller_call()) {
  fn = rlang::call_name(call)
  id = if (is.null(fn)) "unknown" else fn

  cli::cli_warn(
    c( "i" = paste0(
      "The function {.fun {id}} is currently ",
      "considered experimental. Its interface, implementation, and other ",
      "features may change significantly in future versions of the package. ",
      "Use with caution."
    ) ),
    .frequency = "once", .frequency_id = paste0("warn_experimental_", id)
  )
}

url_encode = function(urls) {
  purrr::map_chr(urls, utils::URLencode)
}









