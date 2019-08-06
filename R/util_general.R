
replace_nas = function(cur, rep) {
  cur[is.na(cur)] = rep[is.na(cur)]
  cur
}

flag_experimental = function() {
  calling_func = as.character(sys.calls()[[sys.nframe()-1]])[1]

  usethis::ui_warn( paste0(
    "This function ({usethis::ui_value(calling_func)}) is currently ",
    "considered experimental. Its interface, implementation, and other ",
    "features may change significantly in future versions of the package. ",
    "Use with caution.\n"
  ) )
}








