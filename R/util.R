require_git = function()
{
  git = Sys.which("git")

  if (git == "") {
    stop("git executable not found, if it is installed,",
         "please make sure git can be found using your PATH variable.")
  }

  return(git)
}



styler_available = function() {
  "styler" %in% rownames(installed.packages())
}

empty_result = function(res) {
  length(res) == 1 & all(res == "")
}



# Check for errors that result from using purrr::safely
check_errors = function(res) {
  errs = purrr::map(res, "error")
  purrr::map_lgl(errs, ~!is.null(.x))
}

# Collect errors that result from using purrr::safely
get_errors = function(res) {
  errs = purrr::map(res, "error")
  errs = purrr::discard(errs, is.null)
  purrr::map_chr(errs, as.character)
}

format_list = function(lines, indent=2, indent_char=" ") {
  if (is.numeric(indent))
    indent = paste(rep(indent_char, indent), collapse="")

  lines = sub("\n$", "", lines)
  paste(indent, "*", lines, collapse="\n")
}
