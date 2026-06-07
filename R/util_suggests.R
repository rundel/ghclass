require_pkg = function(pkg, call = rlang::caller_call()) {
  if (requireNamespace(pkg, quietly = TRUE))
    return(invisible())

  fn = rlang::call_name(call)
  if (is.null(fn))
    cli_stop("This function requires the package {.pkg {pkg}} is installed.")
  else
    cli_stop("Function {.fun {fn}} requires the package {.pkg {pkg}} is installed.")
}

require_gert = function(call = rlang::caller_call()) {
  require_pkg("gert", call = call)
}

require_styler = function(call = rlang::caller_call()) {
  require_pkg("styler", call = call)
}
