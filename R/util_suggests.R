require_pkg = function(pkg) {
  func = deparse(sys.calls()[[sys.nframe()-1]])

  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli_stop("Function {.code {func}} requires the package {.pkg {pkg}} is installed.")
  }
}

require_gert = function() {
  require_pkg("gert")
}

require_styler = function() {
  require_pkg("styler")
}


styler_available = function() {
  "styler" %in% rownames(utils::installed.packages())
}
