require_git = function() {
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found using your PATH variable.")
  }

  return(git)
}


styler_available = function() {
  "styler" %in% rownames(utils::installed.packages())
}
