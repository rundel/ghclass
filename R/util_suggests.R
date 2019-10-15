require_git = function() {
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found using your PATH variable.")
  }

  return(git)
}


require_pkg = function(pkg) {
  func = deparse(sys.calls()[[sys.nframe()-1]])

  if (!requireNamespace(pkg, quietly = TRUE)) {
    usethis::ui_stop( paste(
      "Function {usethis::ui_code(func)} requires the package" ,
      "{usethis::ui_code(pkg)} be installed."
    ) )
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
